// Simple people counter using one ultrasonic sensor
// Counts every time someone passes in front of the sensor

byte triggerPin = 5;
byte echoPin = 2;

int currentPeople = 0;
int person = 1;
int baselineDistance = 0;
int sensitivity = 20; //trækker x antal cm fra baslinedistance. f.eks. til den anden side af dørkarm ved montering i en dør
bool personDetected = false;

void setup() {
  Serial.begin(9600);

  pinMode(triggerPin, OUTPUT);
  pinMode(echoPin, INPUT);

  delay(500);
  baselineDistance = measureDistance(); // initial baseline
  Serial.print("Baseline distance: ");
  Serial.println(baselineDistance);
}

void loop() {
  int distance = measureDistance();

  // Detect when something comes closer than baseline - sensitivity
  if (distance < baselineDistance - sensitivity && !personDetected) {
    personDetected = true;
    person;
    //Serial.print("Person detected! Count = ");
    Serial.println(person);
  }

  // Reset detection when area is clear again
  if (distance >= baselineDistance - sensitivity) {
    personDetected = false;
  }

  delay(100); //time between soundwave shocks
}

// Measure distance in cm
int measureDistance() {
  digitalWrite(triggerPin, LOW);
  delayMicroseconds(2);
  digitalWrite(triggerPin, HIGH);
  delayMicroseconds(10);
  digitalWrite(triggerPin, LOW);

  long duration = pulseIn(echoPin, HIGH, 100000);
  return duration / 29 / 2;
}
