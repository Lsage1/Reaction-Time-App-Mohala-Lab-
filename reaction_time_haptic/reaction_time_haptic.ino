/*
 * Arduino Sketch for Reaction Time App
 * Communication via Serial (PySerial)
 * 
 * Protocol:
 * - Receive 'H' from Python: Turn haptic motor ON
 * - Receive 'L' from Python: Turn haptic motor OFF
 * - Send 'OK' back to confirm command received
 * 
 * Upload this sketch to your Arduino before running the Python app
 */

const int HAPTIC_PIN = 13;  // Pin connected to haptic motor (via transistor)

void setup() {
  // Initialize serial communication at 9600 baud
  Serial.begin(9600);
  
  // Set haptic pin as output
  pinMode(HAPTIC_PIN, OUTPUT);
  
  // Ensure motor is off initially
  digitalWrite(HAPTIC_PIN, HIGH);
  
  // Wait for serial connection to establish
  while (!Serial) {
    ; // Wait for serial port to connect
  }
  
  // Send ready message
  Serial.println("READY");
}

void loop() {
  // Check if data is available to read
  if (Serial.available() > 0) {
    // Read the incoming byte
    char command = Serial.read();
    
    // Process command
    switch (command) {
      case 'H':  // Turn haptic motor ON (HIGH)
        digitalWrite(HAPTIC_PIN, LOW);
        Serial.println("ON");
        break;
        
      case 'L':  // Turn haptic motor OFF (LOW)
        digitalWrite(HAPTIC_PIN, HIGH);
        Serial.println("OFF");
        break;
        
      case 'T':  // Test command - blink motor
        digitalWrite(HAPTIC_PIN, LOW);
        delay(200);
        digitalWrite(HAPTIC_PIN, HIGH);
        Serial.println("TEST_OK");
        break;
        
      case '?':  // Status query
        Serial.println("READY");
        break;
        
      default:
        // Unknown command - send error
        Serial.print("ERROR:UNKNOWN_");
        Serial.println(command);
        break;
    }
  }
}

/*
 * Hardware Setup:
 * 
 * Option 1: With NPN Transistor (recommended for motors)
 * --------------------------------------------------------
 * Arduino Pin 9 → 1kΩ Resistor → Transistor Base (e.g., 2N2222)
 * Transistor Collector → Vibration Motor (-)
 * Vibration Motor (+) → 5V (or external power supply)
 * Transistor Emitter → GND
 * 1N4001 Diode across motor (cathode to +, anode to -)
 * 
 * Option 2: With Motor Driver Board (easiest)
 * --------------------------------------------------------
 * Arduino Pin 9 → Motor Driver IN
 * Motor Driver OUT → Vibration Motor
 * Motor Driver VCC → 5V
 * Motor Driver GND → Arduino GND
 * 
 * Option 3: With Relay Module
 * --------------------------------------------------------
 * Arduino Pin 9 → Relay IN
 * Relay VCC → 5V
 * Relay GND → GND
 * Motor connects through relay contacts
 */
