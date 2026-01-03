#!/usr/bin/env python3
"""
Single-Button Reaction Time App - Python Version with PySerial Arduino Support
Measures reaction times with single button response, followed by stimulus identification
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
    def generate_tone(frequency=440, duration=0.5, sample_rate=44100):
        """Generate a WAV file with a sine wave tone"""
        import math
        num_samples = int(sample_rate * duration)
        temp_file = tempfile.NamedTemporaryFile(delete=False, suffix='.wav')
        temp_filename = temp_file.name
        temp_file.close()

        wav_file = wave.open(temp_filename, 'w')
        wav_file.setnchannels(1)
        wav_file.setsampwidth(2)
        wav_file.setframerate(sample_rate)

        for i in range(num_samples):
            value = math.sin(2.0 * math.pi * frequency * i / sample_rate)
            data = int(value * 32767)
            wav_file.writeframes(struct.pack('<h', data))

        wav_file.close()
        return temp_filename

    @staticmethod
    def play_tone(frequency=440, duration=0.5):
        """Play a tone using platform-specific method"""
        wav_file = AudioGenerator.generate_tone(frequency, duration)

        if platform.system() == 'Windows':
            winsound.PlaySound(wav_file, winsound.SND_FILENAME | winsound.SND_ASYNC)
        else:
            try:
                if platform.system() == 'Darwin':
                    subprocess.Popen(['afplay', wav_file],
                                     stdout=subprocess.DEVNULL,
                                     stderr=subprocess.DEVNULL)
                else:
                    subprocess.Popen(['aplay', '-q', wav_file],
                                     stdout=subprocess.DEVNULL,
                                     stderr=subprocess.DEVNULL)
            except Exception as e:
                print(f"Audio playback error: {e}")

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
        self.lock = Lock()

    def detect_arduino_port(self):
        """Auto-detect Arduino port"""
        if not PYSERIAL_AVAILABLE:
            return None

        try:
            ports = serial.tools.list_ports.comports()
            for port in ports:
                if any(keyword in port.description.lower() for keyword in ['arduino', 'ch340', 'usb serial']):
                    print(f"Found potential Arduino: {port.device} - {port.description}")
                    return port.device

            if len(ports) > 0:
                print(f"No Arduino detected, using first available port: {ports[0].device}")
                return ports[0].device
        except Exception as e:
            print(f"Port detection error: {e}")

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
            if self.port is None:
                self.port = self.detect_arduino_port()

            if self.port is None:
                print("No Arduino port found.")
                return False

            print(f"Attempting to connect to Arduino on {self.port}...")

            self.serial_conn = serial.Serial(
                port=self.port,
                baudrate=self.baud_rate,
                timeout=self.timeout,
                write_timeout=self.timeout,
                exclusive=True
            )

            print("Waiting for Arduino to initialize...")
            time.sleep(3)

            self.serial_conn.reset_input_buffer()
            self.serial_conn.reset_output_buffer()
            time.sleep(0.5)

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
                self.serial_conn.write(command.encode())
                self.serial_conn.flush()
                time.sleep(0.1)

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
                self.haptic_off()
                time.sleep(0.1)
                self.serial_conn.close()
                print("Arduino disconnected")
            except Exception as e:
                print(f"Disconnect error: {e}")


class SingleButtonReactionApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Single-Button Reaction Time Experiment")
        self.root.geometry("640x480")
        self.root.resizable(True, True)
        self.root.configure(bg='white')

        # ===== CONFIGURATION =====
        self.SHOW_TRIAL_NUMBER = False
        # =========================

        # Arduino setup
        self.arduino = ArduinoController()
        self.arduino_connected = False

        # Experiment state
        self.current_stimulus = None
        self.trial_list = []
        self.current_trial_index = 0
        self.reaction_time = None
        self.response_button_pressed = False
        self.accepting_responses = False
        self.stimulus_onset_time = None
        self.stimulus_offset_time = None
        self.response_time = None
        self.selected_cues = []

        # Timer objects
        self.timer_obj = None
        self.stim_timer_obj = None
        self.response_timer_obj = None

        # Audio file tracking
        self.current_audio_file = None

        # CSV file tracking
        self.csv_filename = None

        # Phase tracking
        self.current_phase = 'initial_instructions'
        self.exposure_index = 0
        self.in_selection_screen = False  # Track if we're in selection screen

        # Create UI components
        self.create_components()

        # Bind keyboard events
        self.root.bind('<KeyPress>', self.key_pressed)

    def get_next_csv_filename(self):
        """Find the next available CSV filename"""
        base_name = 'one_button_reaction_data'
        extension = '.csv'
        version = 1

        while True:
            filename = f'{base_name}_{version}{extension}'
            if not os.path.isfile(filename):
                return filename
            version += 1

    def create_components(self):
        """Create all UI components"""
        # Progress label (top right)
        self.progress_label = tk.Label(
            self.root,
            text="Trial: 0/70",
            font=("Nunito", 10),
            bg='white',
            fg='black',
            anchor='e'
        )
        if self.SHOW_TRIAL_NUMBER:
            self.progress_label.place(relx=1.0, x=-10, y=21, width=102, height=22, anchor='ne')

        # Visual stimulus panel (yellow rectangle)
        self.visual_stimulus = tk.Frame(
            self.root,
            bg='#FFE864',
            highlightthickness=0
        )
        self.visual_stimulus.place(relx=0.5, rely=0.5, width=260, height=221, anchor='center')
        self.visual_stimulus.place_forget()

        # Instructions text
        self.instructions_text = tk.Label(
            self.root,
            text="",
            font=("Nunito", 20),
            bg='white',
            fg='black',
            wraplength=550,
            justify='left'
        )
        self.instructions_text.place(relx=0.5, rely=0.4, width=550, height=250, anchor='center')
        self.instructions_text.place_forget()

        # Start button
        self.start_button_frame = tk.Frame(self.root, bg='white', highlightthickness=0)
        self.start_button_frame.place(relx=0.5, rely=0.5, width=370, height=115, anchor='center')

        self.start_button_canvas = tk.Canvas(
            self.start_button_frame,
            bg='#00674F',
            highlightthickness=0,
            cursor='hand2'
        )
        self.start_button_canvas.pack(fill=tk.BOTH, expand=True)

        self.start_button_text = self.start_button_canvas.create_text(
            185, 57.5,
            text="Start",
            font=("Nunito", 30, "bold"),
            fill='white'
        )

        self.start_button_canvas.bind('<Button-1>', lambda e: self.start_button_pushed())
        self.start_button = self.start_button_frame

        # Selection screen components (initially hidden)
        self.selection_frame = tk.Frame(self.root, bg='white')
        self.selection_frame.place(relx=0.5, rely=0.5, width=550, height=450, anchor='center')
        self.selection_frame.place_forget()

        # Selection title
        self.selection_title = tk.Label(
            self.selection_frame,
            text="Which cue(s) did you feel?",
            font=("Nunito", 24, "bold"),
            bg='white',
            fg='black'
        )
        self.selection_title.pack(pady=(20, 30))

        # Checkbox variables
        self.visual_var = tk.IntVar()
        self.audio_var = tk.IntVar()
        self.haptic_var = tk.IntVar()

        # Checkboxes with larger font
        checkbox_font = ("Nunito", 18)

        self.visual_check = tk.Checkbutton(
            self.selection_frame,
            text="Visual (Yellow Rectangle) - Press 1",
            variable=self.visual_var,
            font=checkbox_font,
            bg='white',
            fg='black',
            activebackground='white',
            activeforeground='black',
            selectcolor='white',
            highlightthickness=3,
            highlightbackground='black',
            highlightcolor='black',
            borderwidth=3,
            relief='solid',
            indicatoron=1,
            width=30,
            anchor='w'
        )
        self.visual_check.pack(pady=15, anchor='w', padx=50)

        self.audio_check = tk.Checkbutton(
            self.selection_frame,
            text="Auditory (Beep Sound) - Press 2",
            variable=self.audio_var,
            font=checkbox_font,
            bg='white',
            fg='black',
            activebackground='white',
            activeforeground='black',
            selectcolor='white',
            highlightthickness=3,
            highlightbackground='black',
            highlightcolor='black',
            borderwidth=3,
            relief='solid',
            indicatoron=1,
            width=30,
            anchor='w'
        )
        self.audio_check.pack(pady=15, anchor='w', padx=50)

        self.haptic_check = tk.Checkbutton(
            self.selection_frame,
            text="Haptic (Vibration) - Press 3",
            variable=self.haptic_var,
            font=checkbox_font,
            bg='white',
            fg='black',
            activebackground='white',
            activeforeground='black',
            selectcolor='white',
            highlightthickness=3,
            highlightbackground='black',
            highlightcolor='black',
            borderwidth=3,
            relief='solid',
            indicatoron=1,
            width=30,
            anchor='w'
        )
        self.haptic_check.pack(pady=15, anchor='w', padx=50)

        # Continue button
        self.continue_button_frame = tk.Frame(self.selection_frame, bg='white', highlightthickness=0)
        self.continue_button_frame.pack(pady=30)

        self.continue_button_canvas = tk.Canvas(
            self.continue_button_frame,
            bg='#00674F',
            highlightthickness=0,
            cursor='hand2',
            width=320,
            height=70
        )
        self.continue_button_canvas.pack()

        self.continue_button_text = self.continue_button_canvas.create_text(
            160, 35,
            text="Press 4 to Continue",
            font=("Nunito", 20, "bold"),
            fill='white'
        )

        self.continue_button_canvas.bind('<Button-1>', lambda e: self.handle_selection_continue())

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
        """Handle keyboard input"""
        if self.current_phase == 'initial_instructions':
            if event.keysym == '1':
                self.show_stimulus_exposure()

        elif self.current_phase == 'stimulus_exposure':
            # If in selection screen, handle checkbox selection
            if self.in_selection_screen:
                self.handle_selection_key(event.keysym)
            # Otherwise, Key 1 during exposure to practice the response
            elif self.accepting_responses and not self.response_button_pressed:
                if event.keysym == '1':
                    self.record_exposure_response()

        elif self.current_phase == 'intermediate_instructions':
            if event.keysym == '1':
                self.start_actual_experiment()

        elif self.current_phase == 'experiment':
            # If in selection screen, handle checkbox selection
            if self.in_selection_screen:
                self.handle_selection_key(event.keysym)
            # Otherwise, single button (key 1) for response
            elif self.accepting_responses and not self.response_button_pressed:
                if event.keysym == '1':
                    self.record_response()

    def handle_selection_key(self, key):
        """Handle keyboard input during selection screen"""
        if key == '1':
            # Toggle Visual checkbox
            self.visual_var.set(1 - self.visual_var.get())

        elif key == '2':
            # Toggle Auditory checkbox
            self.audio_var.set(1 - self.audio_var.get())

        elif key == '3':
            # Toggle Haptic checkbox
            self.haptic_var.set(1 - self.haptic_var.get())

        elif key == '4':
            # Continue/Submit
            self.handle_selection_continue()

    def show_initial_instructions(self):
        """Show the initial instructions screen"""
        self.current_phase = 'initial_instructions'

        self.start_button.place_forget()
        self.visual_stimulus.place_forget()
        self.progress_label.config(text='')

        instructions = (
            "In this experiment, you will experience three types of cues:\n\n"
            "• Visual (Yellow Rectangle on Screen)\n"
            "• Auditory (Beep Sound)\n"
            "• Haptic (Vibration from Haptic Device)\n\n"
            "Your task is to press KEY 1 as quickly as possible when you detect ANY cue. Place the index finger of your non dominant on the haptic device, and place the fingers of your dominant hand on the external key buttons.\n\n\n\n"
            "After pressing, you'll select which cue(s) you felt.\n\n"
            "Press 1 to see example..."
        )

        self.instructions_text.config(text=instructions, fg='black')
        self.instructions_text.place(relx=0.5, rely=0.45, width=650, height=450, anchor='center')
        self.root.update()

    def show_stimulus_exposure(self):
        """Show each stimulus type one at a time for familiarization"""
        self.current_phase = 'stimulus_exposure'
        self.instructions_text.place_forget()

        exposure_sequence = [
            {'type': 'V', 'name': 'Visual',
             'instruction': 'You will see a YELLOW RECTANGLE.\nPress 1 when you see it.'},
            {'type': 'A', 'name': 'Auditory',
             'instruction': 'You will hear a BEEP.\nPress 1 when you hear it.'},
            {'type': 'H', 'name': 'Haptic',
             'instruction': 'You will feel a VIBRATION.\nPress 1 when you feel it.'}
        ]

        if self.exposure_index >= len(exposure_sequence):
            self.show_intermediate_instructions()
            return

        current_exposure = exposure_sequence[self.exposure_index]

        self.instructions_text.config(text=current_exposure['instruction'], fg='black')
        self.instructions_text.place(relx=0.5, rely=0.3, width=550, height=100, anchor='center')
        self.root.update()

        self.timer_obj = Timer(2.0, lambda: self.present_exposure_stimulus(current_exposure['type']))
        self.timer_obj.start()

    def present_exposure_stimulus(self, stim_type):
        """Present a single stimulus during the exposure phase"""
        self.instructions_text.place_forget()

        # Reset exposure response state
        self.response_button_pressed = False
        self.reaction_time = None
        self.response_time = None

        # Record onset time and enable responses
        self.stimulus_onset_time = time.time()
        self.accepting_responses = True

        try:
            if stim_type == 'V':
                self.visual_stimulus.place(relx=0.5, rely=0.5, width=260, height=221, anchor='center')
            elif stim_type == 'A':
                self.current_audio_file = AudioGenerator.play_tone(440, 0.5)
            elif stim_type == 'H':
                if self.arduino_connected:
                    self.arduino.haptic_on()
        except Exception as e:
            print(f"Error presenting exposure stimulus: {e}")

        # Turn off stimulus after 0.5 seconds
        self.stim_timer_obj = Timer(0.5, self.turn_off_exposure_stimulus)
        self.stim_timer_obj.start()

        # End response window after 1.5 seconds
        self.response_timer_obj = Timer(1.5, self.end_exposure_response_window)
        self.response_timer_obj.start()

    def turn_off_exposure_stimulus(self):
        """Turn off exposure stimulus but continue accepting responses"""
        self.stimulus_offset_time = time.time()

        self.visual_stimulus.place_forget()
        if self.arduino_connected:
            self.arduino.haptic_off()

        self.root.update()

    def record_exposure_response(self):
        """Record the button press response during exposure"""
        if not self.accepting_responses or self.response_button_pressed:
            return

        self.response_button_pressed = True
        self.response_time = time.time()
        self.reaction_time = self.response_time - self.stimulus_onset_time

        # Stop accepting further responses
        self.accepting_responses = False

        # Don't cancel the response timer - let it run until 1.5s
        # The selection screen will show when end_exposure_response_window is called

    def end_exposure_response_window(self):
        """Handle when exposure response window ends without button press"""
        self.accepting_responses = False

        # Always show selection screen after response window ends
        self.show_exposure_selection_screen()

    def show_exposure_selection_screen(self):
        """Show the cue selection screen during exposure"""
        # Hide stimuli
        self.visual_stimulus.place_forget()
        if self.arduino_connected:
            self.arduino.haptic_off()

        # Reset checkboxes
        self.visual_var.set(0)
        self.audio_var.set(0)
        self.haptic_var.set(0)

        # Set selection screen flag
        self.in_selection_screen = True

        # Show selection frame
        self.selection_frame.place(relx=0.5, rely=0.5, width=550, height=450, anchor='center')

        # Force UI update to render properly
        self.root.update_idletasks()
        self.root.update()

    def handle_selection_continue(self):
        """Route to appropriate continue handler based on phase"""
        if self.current_phase == 'stimulus_exposure':
            self.exposure_selection_continue()
        elif self.current_phase == 'experiment':
            self.selection_continue()

    def exposure_selection_continue(self):
        """Handle continue button on exposure selection screen"""
        # Clear selection screen flag
        self.in_selection_screen = False

        # Hide selection screen
        self.selection_frame.place_forget()

        # Move to next exposure
        self.exposure_index += 1

        # Wait 2 seconds before next exposure
        self.timer_obj = Timer(2.0, self.show_stimulus_exposure)
        self.timer_obj.start()

    def show_intermediate_instructions(self):
        """Show instructions before starting the actual experiment"""
        self.current_phase = 'intermediate_instructions'

        self.visual_stimulus.place_forget()

        instructions = (
            "Great! Now you've experienced all three stimulus types.\n\n"
            "In the actual experiment:\n\n"
            "• Press KEY 1 as soon as you detect ANY stimulus\n"
            "• Then select which cue(s) you felt\n"
            "• Stimuli may appear alone or in combination\n"
            "• Respond as quickly as possible!\n\n"
            "Press 1 when you're ready to begin..."
        )

        self.instructions_text.config(text=instructions, fg='black')
        self.instructions_text.place(relx=0.5, rely=0.4, width=550, height=380, anchor='center')
        self.root.update()

    def show_completion_page(self):
        """Show the completion page"""
        self.current_phase = 'completed'

        self.visual_stimulus.place_forget()
        self.selection_frame.place_forget()
        self.progress_label.config(text='')
        self.cleanup_all_timers()
        if self.arduino_connected:
            self.arduino.haptic_off()

        completion_message = (
            "You have completed the testing, thank you for participating.\n\n"
            "Please fill out this survey before notifying your proctor.\n\n"
            "Survey link: [SURVEY URL HERE]"
        )

        self.instructions_text.config(text=completion_message, fg='black', font=("Nunito", 20))
        self.instructions_text.place(relx=0.5, rely=0.45, width=550, height=200, anchor='center')
        self.root.update()

    def start_actual_experiment(self):
        """Start the actual experiment"""
        self.current_phase = 'experiment'

        self.instructions_text.place_forget()
        self.visual_stimulus.place_forget()

        modes = ['V', 'A', 'H', 'VA', 'VH', 'AH', 'VAH']
        self.trial_list = modes * 10
        random.shuffle(self.trial_list)

        self.current_trial_index = 0
        self.start_next_trial()

    def start_stimulus(self):
        """Present the stimulus"""
        self.response_button_pressed = False
        self.reaction_time = None
        self.response_time = None

        self.visual_stimulus.place_forget()
        if self.arduino_connected:
            self.arduino.haptic_off()

        self.root.update()

        self.stimulus_onset_time = time.time()
        self.accepting_responses = True

        try:
            if self.current_stimulus == 'V':
                self.visual_stimulus.place(relx=0.5, rely=0.5, width=260, height=221, anchor='center')
            elif self.current_stimulus == 'A':
                self.current_audio_file = AudioGenerator.play_tone(440, 0.5)
            elif self.current_stimulus == 'H':
                if self.arduino_connected:
                    self.arduino.haptic_on()
            elif self.current_stimulus == 'VA':
                self.visual_stimulus.place(relx=0.5, rely=0.5, width=260, height=221, anchor='center')
                self.current_audio_file = AudioGenerator.play_tone(440, 0.5)
            elif self.current_stimulus == 'VH':
                self.visual_stimulus.place(relx=0.5, rely=0.5, width=260, height=221, anchor='center')
                if self.arduino_connected:
                    self.arduino.haptic_on()
            elif self.current_stimulus == 'AH':
                self.current_audio_file = AudioGenerator.play_tone(440, 0.5)
                if self.arduino_connected:
                    self.arduino.haptic_on()
            elif self.current_stimulus == 'VAH':
                self.visual_stimulus.place(relx=0.5, rely=0.5, width=260, height=221, anchor='center')
                self.current_audio_file = AudioGenerator.play_tone(440, 0.5)
                if self.arduino_connected:
                    self.arduino.haptic_on()
        except Exception as e:
            print(f"Error starting stimulus: {e}")

        self.root.update()

        self.stim_timer_obj = Timer(0.5, self.turn_off_stimulus)
        self.stim_timer_obj.start()

        self.response_timer_obj = Timer(1.5, self.end_response_window)
        self.response_timer_obj.start()

    def turn_off_stimulus(self):
        """Turn off stimuli but continue accepting responses"""
        self.stimulus_offset_time = time.time()

        self.visual_stimulus.place_forget()
        if self.arduino_connected:
            self.arduino.haptic_off()

        self.root.update()

    def record_response(self):
        """Record the button press response"""
        if not self.accepting_responses or self.response_button_pressed:
            return

        self.response_button_pressed = True
        self.response_time = time.time()
        self.reaction_time = self.response_time - self.stimulus_onset_time

        # Stop accepting further responses
        self.accepting_responses = False

        # Don't cancel the response timer - let it run until 1.5s
        # The selection screen will show when end_response_window is called

    def end_response_window(self):
        """Handle when response window ends without button press"""
        self.accepting_responses = False

        # Always show selection screen after response window ends
        self.show_selection_screen()

    def show_selection_screen(self):
        """Show the cue selection screen"""
        # Hide stimuli
        self.visual_stimulus.place_forget()
        if self.arduino_connected:
            self.arduino.haptic_off()

        # Reset checkboxes
        self.visual_var.set(0)
        self.audio_var.set(0)
        self.haptic_var.set(0)

        # Set selection screen flag
        self.in_selection_screen = True

        # Show selection frame
        self.selection_frame.place(relx=0.5, rely=0.5, width=550, height=450, anchor='center')

        # Force UI update to render properly
        self.root.update_idletasks()
        self.root.update()

    def selection_continue(self):
        """Handle continue button on selection screen"""
        # Clear selection screen flag
        self.in_selection_screen = False

        # Collect selected cues
        self.selected_cues = []
        if self.visual_var.get():
            self.selected_cues.append('V')
        if self.audio_var.get():
            self.selected_cues.append('A')
        if self.haptic_var.get():
            self.selected_cues.append('H')

        # Hide selection screen
        self.selection_frame.place_forget()

        # Save data
        self.save_data()

        # Move to next trial
        self.current_trial_index += 1

        # Schedule next trial
        delay = 2 + random.random() * 3
        self.timer_obj = Timer(delay, self.start_next_trial)
        self.timer_obj.start()

    def save_data(self):
        """Save trial data to CSV file"""
        csv_file = self.csv_filename
        file_exists = os.path.isfile(csv_file)

        onset_datetime = datetime.fromtimestamp(self.stimulus_onset_time).strftime('%Y-%m-%d %H:%M:%S.%f')
        offset_datetime = datetime.fromtimestamp(self.stimulus_offset_time).strftime('%Y-%m-%d %H:%M:%S.%f')

        # Format reaction time
        rt_str = f'{self.reaction_time:.3f}' if self.reaction_time is not None else ''

        # Format selected cues
        selected_str = ''.join(sorted(self.selected_cues)) if self.selected_cues else ''

        # Determine correctness
        correct_keys_map = {
            'V': ['V'],
            'A': ['A'],
            'H': ['H'],
            'VA': ['V', 'A'],
            'VH': ['V', 'H'],
            'AH': ['A', 'H'],
            'VAH': ['V', 'A', 'H']
        }

        correct_keys = correct_keys_map.get(self.current_stimulus, [])
        felt_correctly = 'YES' if sorted(self.selected_cues) == sorted(correct_keys) else 'NO'

        try:
            with open(csv_file, 'a', newline='') as f:
                writer = csv.writer(f)

                if not file_exists:
                    writer.writerow([
                        'TrialEndTime',
                        'StimulusOnsetTime',
                        'StimulusOffsetTime',
                        'Stimulus',
                        'SelectedCues',
                        'FeltCorrectly',
                        'ReactionTime_seconds'
                    ])

                writer.writerow([
                    datetime.now().strftime('%Y-%m-%d %H:%M:%S.%f'),
                    onset_datetime,
                    offset_datetime,
                    self.current_stimulus,
                    selected_str,
                    felt_correctly,
                    rt_str
                ])
        except Exception as e:
            print(f"Failed to write CSV: {e}")

    def start_next_trial(self):
        """Start the next trial or finish experiment"""
        if self.current_trial_index >= len(self.trial_list):
            self.show_completion_page()
            return

        self.accepting_responses = False
        self.visual_stimulus.place_forget()

        self.current_stimulus = self.trial_list[self.current_trial_index]

        if self.SHOW_TRIAL_NUMBER:
            self.progress_label.config(text=f'Trial {self.current_trial_index + 1} of {len(self.trial_list)}')

        delay = 2 + random.random() * 3
        self.timer_obj = Timer(delay, self.start_stimulus)
        self.timer_obj.start()

    def start_button_pushed(self):
        """Handle start button click"""
        self.cleanup_all_timers()

        self.csv_filename = self.get_next_csv_filename()
        print(f"Data will be saved to: {self.csv_filename}")

        self.arduino_connected = self.arduino.connect()

        if self.arduino_connected:
            print("Testing haptic motor...")
            response = self.arduino.test_haptic()
            if response:
                print(f"Motor test response: {response}")
            time.sleep(0.5)

        self.show_initial_instructions()

    def cleanup(self):
        """Cleanup before closing"""
        self.cleanup_all_timers()

        if self.arduino_connected:
            self.arduino.disconnect()


def main():
    """Main entry point"""
    root = tk.Tk()
    app = SingleButtonReactionApp(root)

    def on_closing():
        app.cleanup()
        root.destroy()

    root.protocol("WM_DELETE_WINDOW", on_closing)
    root.mainloop()


if __name__ == "__main__":
    main()