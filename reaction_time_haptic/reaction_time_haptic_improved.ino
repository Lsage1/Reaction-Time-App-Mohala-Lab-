/*
 * Arduino Sketch for Reaction Time App
 * Communication via Serial (PySerial)
 * IMPROVED VERSION - More robust to timing and serial noise
 * 
 * Protocol:
 * - Receive 'H' from Python: Turn haptic motor ON
 * - Receive 'L' from Python: Turn haptic motor OFF
 * - Send 'OK' back to confirm command received
 * 
 * Upload this sketch to your Arduino before running the Python app
 */

const int HAPTIC_PIN = 8;  // Pin connected to haptic motor (D8)

void setup() {
  // Initialize serial communication at 9600 baud
  Serial.begin(9600);
  
  // Set haptic pin as output
  pinMode(HAPTIC_PIN, OUTPUT);
  
  // Ensure motor is off initially
  digitalWrite(HAPTIC_PIN, LOW);
  
  // Wait for serial connection to establish
  while (!Serial) {
    ; // Wait for serial port to connect
  }
  
  // Additional delay for stability
  delay(2000);
  
  // Clear any garbage in the serial buffer
  while (Serial.available() > 0) {
    Serial.read();
  }
  
  // Send ready message
  Serial.println("READY");
}

void loop() {
  // Check if data is available to read
  if (Serial.available() > 0) {
    // Read the incoming byte
    char command = Serial.read();
    
    // Clear any remaining junk in buffer
    delay(10);
    while (Serial.available() > 0) {
      Serial.read();
    }
    
    // Process command
    switch (command) {
      case 'H':  // Turn haptic motor ON (HIGH)
        digitalWrite(HAPTIC_PIN, HIGH);
        Serial.println("ON");
        break;
        
      case 'L':  // Turn haptic motor OFF (LOW)
        digitalWrite(HAPTIC_PIN, LOW);
        Serial.println("OFF");
        break;
        
      case 'T':  // Test command - blink motor
        digitalWrite(HAPTIC_PIN, HIGH);
        delay(200);
        digitalWrite(HAPTIC_PIN, LOW);
        Serial.println("TEST_OK");
        break;
        
      case '?':  // Status query
        Serial.println("READY");
        break;
        
      case '\n':  // Ignore newlines
      case '\r':  // Ignore carriage returns
      case ' ':   // Ignore spaces
        // Do nothing for whitespace
        break;
        
      default:
        // Unknown command - but don't send error for control characters
        if (command >= 32 && command <= 126) {
          Serial.print("ERROR:UNKNOWN_");
          Serial.println(command);
        }
        break;
    }
  }
}

/*
 * Hardware Setup:
 * 
 * Arduino Nano with Haptic Motor on Pin D8
 * --------------------------------------------------------
 * 
 * Option 1: With NPN Transistor (recommended for motors)
 * --------------------------------------------------------
 * Arduino Pin D8 → 1kΩ Resistor → Transistor Base (e.g., 2N2222)
 * Transistor Collector → Vibration Motor (-)
 * Vibration Motor (+) → 5V (or external power supply)
 * Transistor Emitter → GND
 * 1N4001 Diode across motor (cathode to +, anode to -)
 * 
 * Option 2: With Motor Driver Board (easiest)
 * --------------------------------------------------------
 * Arduino Pin D8 → Motor Driver IN
 * Motor Driver OUT → Vibration Motor
 * Motor Driver VCC → 5V
 * Motor Driver GND → Arduino GND
 * 
 * Option 3: With Relay Module
 * --------------------------------------------------------
 * Arduino Pin D8 → Relay IN
 * Relay VCC → 5V
 * Relay GND → GND
 * Motor connects through relay contacts
 * 
 * Option 4: Direct Connection (ONLY for small coin motors < 20mA)
 * --------------------------------------------------------
 * Arduino Pin D8 → Vibration Motor (+)
 * Vibration Motor (-) → GND
 * NOTE: This may not work for larger motors!
 */
