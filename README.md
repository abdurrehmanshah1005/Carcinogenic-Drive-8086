# Carcinogenic-Drive-8086
A vertical scrolling car racing game developed in 8086 Assembly Language (COAL Semester Project)
# Carcinogenic Drive - 8086 Assembly Game

**Course:** Computer Organization & Assembly Language (COAL)  
**Project:** Semester Final  
**Tech Stack:** 8086 Assembly (NASM), VGA Mode 13h, DOSBox  

## 🏎️ Project Overview
Carcinogenic Drive is a high-speed, vertical scrolling racing game developed entirely in 16-bit 8086 Assembly. The player controls a sports car, navigating through traffic, collecting coins, and managing fuel consumption while avoiding high-speed collisions.

## 🎥 Gameplay Demo
[Link to your YouTube Screen Recording goes here]

## ✨ Key Features
* **Double Buffering:** Implemented a custom back-buffer to eliminate screen flickering during rendering.
* **Asset Loading:** Loads external binary (`.bin`) image files for high-quality menus and sprites.
* **Lane Logic:** Smooth lane-switching mechanism with boundary checks.
* **Collision Detection:** Bounding-box collision logic for cars, coins, and fuel cans.
* **Sound Engine:** Custom PIT (Programmable Interval Timer) driver for background music ("Highway Blues") and crash sound effects.
* **Dynamic Difficulty:** Game speed and NPC spawn rates increase as the player survives longer.

## 🎮 How to Play
1.  **Start:** Run `main.exe` in DOSBox.
2.  **Input:** Enter your Name and Roll Number on the info screen.
3.  **Controls:**
    * `Arrow Keys`: Move Up, Down, Left, Right.
    * `ESC`: Pause Game.
4.  **Objective:** Collect coins for score, pick up Jerry Cans for fuel, and avoid the Blue NPC cars.

## 🛠️ Technical Details
* **Video Mode:** VGA 13h (320x200, 256 colors).
* **Memory Management:** Direct writing to Video Memory (`0xA000`) and distinct segments for back-buffering.
* **Interrupts Used:**
    * `INT 10h`: Video mode selection.
    * `INT 21h`: DOS file handling (opening/reading assets).
    * `INT 16h`: Keyboard input polling.
    * `0x40:006C`: Reading the BIOS timer tick for frame timing.

## 👥 Contributors
* **Member 1:** ABD UR REHMAN SHAH 24L-0712 - Phases 1, 3, 5
* **Member 2:** SHAJEE HAROON 24L-0712 - Phases 2, 4, 6
