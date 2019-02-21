# Deep-Learning-in-Scala
The project is aimed at implementing a two-layered convolutional neural network using a purely functional approach.

## Installation
You need to install scala as a prerequisite to run these files. For Ubuntu 16.04:
```bash
sudo apt-get install scala
```
## Usage
Compile the files using:
```bash
scalac main.scala driver.scala
```
And run it as:
```bash
scala main.Driver t*.in > t*.check
```
You can check your output using:
```bash
diff t*.check t*.out
```
