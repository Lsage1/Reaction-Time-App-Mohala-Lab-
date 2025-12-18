#!/usr/bin/env python3
"""
Reaction Time App - Python Version with PySerial Arduino Support
Measures reaction times to visual, auditory, and haptic stimuli
"""

import tkinter as tk
from tkinter import ttk
import random
import time
from datetime import datetime
import csv
import os
from threading import Timer, Lock
import platform
import wave
import struct
import tempfile

# Platform-specific audio imports
if platform.system() == 'Windows':
    import winsound
else:
    try:
        import subprocess

        AUDIO_PLAYER = 'afplay' if platform.system() == 'Darwin' else 'aplay'
    except:
        print("Warning: Audio playback may not work on this system")

# Arduino support with PySerial
try:
    import serial
    import serial.tools.list_ports

    PYSERIAL_AVAILABLE = True
except ImportError:
    PYSERIAL_AVAILABLE = False
    print("PySerial not installed. Haptic feedback disabled.")
    print("To enable: pip install pyserial")


class AudioGenerator:
    """Simple audio tone generator using built-in libraries"""

    @staticmethod
    def generate_tone(frequency=440, duration=0.4, sample_rate=44100):
        """Generate a WAV file with a sine wave tone"""
        import math

        num_samples = int(sample_rate * duration)

        # Create temporary WAV file
        temp_file = tempfile.NamedTemporaryFile(delete=False, suffix='.wav')
        temp_filename = temp_file.name
        temp_file.close()

        # Generate sine wave samples
        wav_file = wave.open(temp_filename, 'w')
        wav_file.setnchannels(1)  # Mono
        wav_file.setsampwidth(2)  # 2 bytes per sample (16-bit)
        wav_file.setframerate(sample_rate)

        for i in range(num_samples):
            # Generate sine wave value
            value = math.sin(2.0 * math.pi * frequency * i / sample_rate)
            # Scale to 16-bit integer range
            data = int(value * 32767)
            # Pack as binary data
            wav_file.writeframes(struct.pack('<h', data))

        wav_file.close()
        return temp_filename

    @staticmethod
    def play_tone(frequency=440, duration=0.4):
        """Play a tone using platform-specific method"""
        wav_file = AudioGenerator.generate_tone(frequency, duration)

        if platform.system() == 'Windows':
            # Use winsound on Windows
            winsound.PlaySound(wav_file, winsound.SND_FILENAME | winsound.SND_ASYNC)
        else:
            # Use system audio player on Unix-like systems
            try:
                if platform.system() == 'Darwin':
                    # macOS
                    subprocess.Popen(['afplay', wav_file],
                                     stdout=subprocess.DEVNULL,
                                     stderr=subprocess.DEVNULL)
                else:
                    # Linux
                    subprocess.Popen(['aplay', '-q', wav_file],
                                     stdout=subprocess.DEVNULL,
                                     stderr=subprocess.DEVNULL)
            except Exception as e:
                print(f"Audio playback error: {e}")

        # Schedule cleanup of temp file after playback
        Timer(duration + 0.5, lambda: AudioGenerator.cleanup_file(wav_file)).start()

        return wav_file

    @staticmethod
    def cleanup_file(filename):
        """Delete temporary audio file"""
        try:
            os.remove(filename)
        except:
            pass


class ArduinoController:
    """Arduino controller using PySerial"""

    def __init__(self, port=None, baud_rate=9600, timeout=1):
        self.serial_conn = None
        self.port = port
        self.baud_rate = baud_rate
        self.timeout = timeout
        self.lock = Lock()  # Thread safety for serial communication

    def detect_arduino_port(self):
        """Auto-detect Arduino port"""
        if not PYSERIAL_AVAILABLE:
            return None

        try:
            ports = serial.tools.list_ports.comports()

            # Look for Arduino-like devices
            for port in ports:
                # Check for Arduino in description
                if any(keyword in port.description.lower() for keyword in ['arduino', 'ch340', 'usb serial']):
                    print(f"Found potential Arduino: {port.device} - {port.description}")
                    return port.device

            # If no Arduino found, return first available port
            if len(ports) > 0:
                print(f"No Arduino detected, using first available port: {ports[0].device}")
                return ports[0].device

        except Exception as e:
            print(f"Port detection error: {e}")

        # Default ports by platform
        if platform.system() == 'Windows':
            return 'COM3'
        elif platform.system() == 'Darwin':
            return '/dev/tty.usbmodem14101'
        else:
            return '/dev/ttyACM0'

    def connect(self):
        """Connect to Arduino"""
        if not PYSERIAL_AVAILABLE:
            print("PySerial not available. Cannot connect to Arduino.")
            return False

        try:
            # Auto-detect port if not specified
            if self.port is None:
                self.port = self.detect_arduino_port()

            if self.port is None:
                print("No Arduino port found.")
                return False

            print(f"Attempting to connect to Arduino on {self.port}...")

            # Open serial connection
            self.serial_conn = serial.Serial(
                port=self.port,
                baudrate=self.baud_rate,
                timeout=self.timeout,
                write_timeout=self.timeout,
                exclusive=True  # Windows exclusive access
            )

            # Wait for Arduino to initialize (important!)
            print("Waiting for Arduino to initialize...")
            time.sleep(3)  # Longer wait for stability

            # Clear any initial data
            self.serial_conn.reset_input_buffer()
            self.serial_conn.reset_output_buffer()

            # Additional delay for Windows
            time.sleep(0.5)

            # Test connection with status query
            print("Testing connection...")
            response = self.send_command('?')
            if response and 'READY' in response:
                print(f"✓ Arduino connected successfully on {self.port}")
                return True
            else:
                print(f"✓ Arduino connected on {self.port} (no handshake, but ready)")
                return True

        except serial.SerialException as e:
            print(f"✗ Serial connection error: {e}")
            self.serial_conn = None
            return False
        except Exception as e:
            print(f"✗ Arduino connection failed: {e}")
            self.serial_conn = None
            return False

    def send_command(self, command):
        """Send command to Arduino and read response"""
        if self.serial_conn is None or not self.serial_conn.is_open:
            return None

        with self.lock:
            try:
                # Send command
                self.serial_conn.write(command.encode())
                self.serial_conn.flush()

                # Delay for Arduino to process (critical for Windows)
                time.sleep(0.1)

                # Read response if available
                if self.serial_conn.in_waiting > 0:
                    response = self.serial_conn.readline().decode('utf-8', errors='ignore').strip()
                    return response

                return None

            except serial.SerialException as e:
                print(f"Serial communication error: {e}")
                return None
            except Exception as e:
                print(f"Communication error: {e}")
                return None

    def haptic_on(self):
        """Turn haptic motor ON"""
        response = self.send_command('H')
        return response

    def haptic_off(self):
        """Turn haptic motor OFF"""
        response = self.send_command('L')
        return response

    def test_haptic(self):
        """Test haptic motor with brief pulse"""
        response = self.send_command('T')
        return response

    def disconnect(self):
        """Close serial connection"""
        if self.serial_conn is not None and self.serial_conn.is_open:
            try:
                # Turn off haptic before disconnecting
                self.haptic_off()
                time.sleep(0.1)
                self.serial_conn.close()
                print("Arduino disconnected")
            except Exception as e:
                print(f"Disconnect error: {e}")


class ReactionTimeApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Reaction Time Experiment")
        self.root.geometry("640x480")
        self.root.configure(bg='white')

        # Arduino setup with PySerial
        self.arduino = ArduinoController()
        self.arduino_connected = False

        # Experiment state
        self.current_stimulus = None
        self.trial_list = []
        self.current_trial_index = 0
        self.keys_pressed = []
        self.reaction_times = []
        self.accepting_responses = False
        self.stimulus_end_time = None

        # Timer objects
        self.timer_obj = None
        self.stim_timer_obj = None
        self.response_timer_obj = None

        # Audio file tracking
        self.current_audio_file = None

        # Create UI components
        self.create_components()

        # Bind keyboard events
        self.root.bind('<KeyPress>', self.key_pressed)

    def create_components(self):
        """Create all UI components"""
        # Reaction time label (top left)
        self.reaction_time_label = tk.Label(
            self.root,
            text="Reaction Time:",
            font=("Arial", 10),
            bg='white',
            anchor='w'
        )
        self.reaction_time_label.place(x=21, y=21, width=162, height=22)

        # Progress label (top right)
        self.progress_label = tk.Label(
            self.root,
            text="Trial: 0/70",
            font=("Arial", 10),
            bg='white',
            anchor='e'
        )
        self.progress_label.place(x=517, y=21, width=102, height=22)

        # Visual stimulus panel (yellow rectangle, initially hidden)
        self.visual_stimulus = tk.Frame(
            self.root,
            bg='#FFE864',  # Yellow color
            highlightthickness=0
        )
        self.visual_stimulus.place(x=191, y=131, width=260, height=221)
        self.visual_stimulus.place_forget()  # Hide initially

        # Stimulus type label (bottom center)
        self.stimulus_type_label = tk.Label(
            self.root,
            text="Stimulus Type",
            font=("Arial", 10),
            bg='white'
        )
        self.stimulus_type_label.place(x=281, y=439, width=80, height=22)

        # Start button (center)
        self.start_button = tk.Button(
            self.root,
            text="Start",
            font=("Arial", 30, "bold"),
            bg='#00663E',  # Dark green
            fg='white',
            command=self.start_button_pushed,
            relief=tk.FLAT
        )
        self.start_button.place(x=197, y=203, width=247, height=77)

    def cleanup_all_timers(self):
        """Cancel all active timers"""
        if self.timer_obj is not None:
            self.timer_obj.cancel()
            self.timer_obj = None
        if self.stim_timer_obj is not None:
            self.stim_timer_obj.cancel()
            self.stim_timer_obj = None
        if self.response_timer_obj is not None:
            self.response_timer_obj.cancel()
            self.response_timer_obj = None

    def key_pressed(self, event):
        """Handle keyboard input during response window"""
        if not self.accepting_responses:
            return

        # Only accept keys 1, 2, 3
        if event.char in ['1', '2', '3']:
            # Record reaction time
            rt = time.time() - self.stimulus_end_time

            # Store key and RT
            self.keys_pressed.append(event.char)
            self.reaction_times.append(rt)

    # --- STATE 1: Start Stimulus ---
    def start_stimulus(self):
        """Present the stimulus"""
        # Reset trial data
        self.keys_pressed = []
        self.reaction_times = []
        self.reaction_time_label.config(text='')
        self.stimulus_type_label.config(text=self.current_stimulus)

        # Disable responses
        self.accepting_responses = False

        # Turn off visual/haptic initially
        self.visual_stimulus.place_forget()
        if self.arduino_connected:
            self.arduino.haptic_off()

        self.root.update()

        # Present stimulus based on type
        try:
            if self.current_stimulus == 'V':
                self.visual_stimulus.place(x=191, y=131, width=260, height=221)

            elif self.current_stimulus == 'A':
                self.current_audio_file = AudioGenerator.play_tone(440, 0.4)

            elif self.current_stimulus == 'H':
                if self.arduino_connected:
                    self.arduino.haptic_on()

            elif self.current_stimulus == 'VA':
                self.visual_stimulus.place(x=191, y=131, width=260, height=221)
                self.current_audio_file = AudioGenerator.play_tone(440, 0.4)

            elif self.current_stimulus == 'VH':
                self.visual_stimulus.place(x=191, y=131, width=260, height=221)
                if self.arduino_connected:
                    self.arduino.haptic_on()

            elif self.current_stimulus == 'AH':
                self.current_audio_file = AudioGenerator.play_tone(440, 0.4)
                if self.arduino_connected:
                    self.arduino.haptic_on()

            elif self.current_stimulus == 'VAH':
                self.visual_stimulus.place(x=191, y=131, width=260, height=221)
                self.current_audio_file = AudioGenerator.play_tone(440, 0.4)
                if self.arduino_connected:
                    self.arduino.haptic_on()

        except Exception as e:
            print(f"Error starting stimulus: {e}")

        self.root.update()

        # Schedule stimulus offset after 0.5 seconds
        self.stim_timer_obj = Timer(0.5, self.begin_response_window)
        self.stim_timer_obj.start()

    # --- STATE 2: Begin Response Window ---
    def begin_response_window(self):
        """Turn off stimuli and start accepting responses"""
        # Turn off all stimuli
        self.visual_stimulus.place_forget()
        if self.arduino_connected:
            self.arduino.haptic_off()

        self.root.update()

        # Start accepting responses
        self.accepting_responses = True
        self.stimulus_end_time = time.time()

        # Schedule end of response window after 1.5 seconds
        self.response_timer_obj = Timer(1.5, self.end_response_window)
        self.response_timer_obj.start()

    # --- STATE 3: End Response Window ---
    def end_response_window(self):
        """Stop accepting responses and save data"""
        self.accepting_responses = False

        # Determine correct keys
        correct_keys_map = {
            'V': ['1'],
            'A': ['2'],
            'H': ['3'],
            'VA': ['1', '2'],
            'VH': ['1', '3'],
            'AH': ['2', '3'],
            'VAH': ['1', '2', '3']
        }

        correct_keys = correct_keys_map.get(self.current_stimulus, [])

        # Check if response was correct
        if sorted(self.keys_pressed) == sorted(correct_keys):
            felt_correctly = 'YES'
        else:
            felt_correctly = 'NO'

        # Format data for CSV
        if len(self.keys_pressed) == 0:
            keys_str = ''
            rt_str = ''
        else:
            keys_str = ','.join(self.keys_pressed)
            rt_str = ','.join([f'{rt:.3f}' for rt in self.reaction_times])

        # Save to CSV
        self.save_data(keys_str, felt_correctly, rt_str)

        # Move to next trial
        self.current_trial_index += 1

        # Schedule next trial with random delay (2-5 seconds)
        delay = 2 + random.random() * 3
        self.timer_obj = Timer(delay, self.start_next_trial)
        self.timer_obj.start()

    def save_data(self, keys_str, felt_correctly, rt_str):
        """Save trial data to CSV file"""
        csv_file = 'reaction_data.csv'

        # Check if file exists to determine if we need headers
        file_exists = os.path.isfile(csv_file)

        try:
            with open(csv_file, 'a', newline='') as f:
                writer = csv.writer(f)

                # Write header if new file
                if not file_exists:
                    writer.writerow(['Time', 'Stimulus', 'KeysPressed', 'FeltCorrectly', 'ReactionTimes'])

                # Write data
                writer.writerow([
                    datetime.now().strftime('%Y-%m-%d %H:%M:%S.%f'),
                    self.current_stimulus,
                    keys_str,
                    felt_correctly,
                    rt_str
                ])
        except Exception as e:
            print(f"Failed to write CSV: {e}")

    # --- STATE 4: Start Next Trial ---
    def start_next_trial(self):
        """Start the next trial or finish experiment"""
        # Check if experiment is finished
        if self.current_trial_index >= len(self.trial_list):
            self.stimulus_type_label.config(text='Experiment Complete!')
            self.reaction_time_label.config(text='')
            self.visual_stimulus.place_forget()
            self.progress_label.config(text=f'Trial {len(self.trial_list)} of {len(self.trial_list)}')
            self.cleanup_all_timers()
            if self.arduino_connected:
                self.arduino.haptic_off()
            return

        # Reset UI
        self.accepting_responses = False
        self.visual_stimulus.place_forget()
        self.reaction_time_label.config(text='')
        self.stimulus_type_label.config(text='')

        # Set current stimulus
        self.current_stimulus = self.trial_list[self.current_trial_index]
        self.progress_label.config(text=f'Trial {self.current_trial_index + 1} of {len(self.trial_list)}')

        # Random delay before stimulus (1.5-3.5 seconds)
        delay = 1.5 + random.random() * 2

        # Schedule stimulus
        self.timer_obj = Timer(delay, self.start_stimulus)
        self.timer_obj.start()

    def start_button_pushed(self):
        """Handle start button click"""
        # Clean up any existing timers
        self.cleanup_all_timers()

        # Initialize Arduino
        self.arduino_connected = self.arduino.connect()

        # Test haptic if connected
        if self.arduino_connected:
            print("Testing haptic motor...")
            response = self.arduino.test_haptic()
            if response:
                print(f"Motor test response: {response}")
            time.sleep(0.5)  # Brief pause after test

        # Hide start button
        self.start_button.place_forget()

        # Reset UI
        self.visual_stimulus.place_forget()
        self.stimulus_type_label.config(text='')
        self.reaction_time_label.config(text='')

        # Generate trial list
        modes = ['V', 'A', 'H', 'VA', 'VH', 'AH', 'VAH']
        self.trial_list = modes * 10  # 70 trials total
        random.shuffle(self.trial_list)

        self.current_trial_index = 0

        # Start first trial
        self.start_next_trial()

    def cleanup(self):
        """Cleanup before closing"""
        self.cleanup_all_timers()

        # Disconnect Arduino
        if self.arduino_connected:
            self.arduino.disconnect()


def main():
    """Main entry point"""
    root = tk.Tk()
    app = ReactionTimeApp(root)

    # Handle window close
    def on_closing():
        app.cleanup()
        root.destroy()

    root.protocol("WM_DELETE_WINDOW", on_closing)
    root.mainloop()


if __name__ == "__main__":
    main()