clc
clear arduinoObj

arduinoObj = arduino("COM15", "Uno", Libraries = ["I2C","SPI","Servo"])

ledPin = 'D13';

% Blink loop
for i = 1:10      % blink 10 times
    writeDigitalPin(arduinoObj, ledPin, 1);  % Turn LED ON
    pause(0.5);                     % Wait 0.5 seconds
    writeDigitalPin(arduinoObj, ledPin, 0);  % Turn LED OFF
    pause(0.5); 
end

clear arduinoObj

fprintf("done")