---
title: Auto Coach! My first commerical AI Project!
description: Explains the project, how it worked. Example code in Python and C++
slug: ai-autocoach
date: 2020-12-03
image: post/Articles/IMAGES/hal6.jpg
draft: false
weight: 7
categories:
  - Artificial Intellgence
  - History
  - Languages
  - Machine Learning
  - Python
  - CPP
tags:
  - AI
  - LISP
  - Prolog
  - Artificial
  - Intelligence
  - History
  - Logic
  - Programming
  - Machine
  - Learning
  - Neural
  - Networks
  - Expert
  - Systems
  - Python
  - CPP
lastmod: 2025-02-16T23:24:04.615Z
---
# Gun Shooting Neural Network Coach with OpenCV

## **How the project came to be ...**

ME!  With no formal training in the AI Space , but always a fascination with things like LISP and Prolog, I was asked to build a AI System to improve firearms shooting in students.

I learned LISP and Prolog when I was younger.. I saw the movie 2001 and wanted to make my own HAL..

but 8 bit PCs at the time just didn't have the horse power.... :)

And I knew about Neural Networks having read and played here and there.

AND i used to own a palm pilot which had pretty good neural network based handwriting recognition built in ..

So i took on this project with enthusiasm and learned a lot in the process...

**so this was the project:**

## Auto-Coach

Build a virtual firearm shooting coach powered by AI.

Auto-Coach's job is to be a firearms instructor who never gets tired and can give real-time feedback.

After being first train by the human expert of course.. :)

The system needed to :

1. **Capture real-time sensor data**: Gun angle, trigger movement, and stability.
2. **Train a Neural Network**: Use data trained by an expert firearms shooter .
3. **Run in Student Mode**: Evaluate and provide feedback to students, based on the expert's captured data.

## Toy Example to play with

**BELOW is not the code from that project. Just an example to demonstrate the ideas..**

**Main code is Python, the original project was C++**\
(I put a C++ example at the end btw...)

For this code we will use OpenCV's `cv2.ml.ANN_MLP` for the neural network and Python to make it all work.

***

NOTE:

ALSO:\
When I did this OpenCV was **DA BOMB!** but now days the Machine Learning space has a ton more options to choose from ...

On with the toy code and how it works

***

## **Auto-Coach Project Phases**

### 1. **Data Collection**

We need to collect from the Human Expert while shooting:

* **Gun angle**: (pitch, yaw, roll) over time.
* **Trigger pressure**: Pressure changes during the trigger pull.
* **Shot score**: Target result (0–100).

***

### 2. **Code Implementation**

Install OpenCV:

```bash
pip install opencv-python
```

### **Python Code**

```python
import cv2
import numpy as np
import random

# Simulated sensor data generator for training
def generate_sensor_data(shooter_type="expert", shots=1000):
    data = []
    labels = []
    for _ in range(shots):
        # Simulate gun angle steadiness
        angle_x = np.random.normal(0 if shooter_type == "expert" else 5, 1)
        angle_y = np.random.normal(0 if shooter_type == "expert" else 5, 1)
        angle_z = np.random.normal(0 if shooter_type == "expert" else 5, 1)

        # Simulate trigger pull smoothness
        trigger_smoothness = np.random.normal(0.5 if shooter_type == "expert" else 1.5, 0.2)

        # Simulate trigger pull time
        trigger_pull_time = np.random.normal(1 if shooter_type == "expert" else 0.5, 0.1)

        # Calculate shot score
        base_score = 100 - abs(angle_x) - abs(angle_y) - abs(angle_z) \
                     - trigger_smoothness*5 - abs(trigger_pull_time - 1)*10
        shot_score = max(0, min(100, base_score))

        # Feature vector
        features = np.array([angle_x, angle_y, angle_z, trigger_smoothness, trigger_pull_time], dtype=np.float32)

        data.append(features)
        labels.append([shot_score])

    return np.array(data, dtype=np.float32), np.array(labels, dtype=np.float32)

# Generate training data
expert_data, expert_labels = generate_sensor_data("expert", 1000)
student_data, student_labels = generate_sensor_data("student", 1000)

# Combine datasets
train_data = np.vstack((expert_data, student_data))
train_labels = np.vstack((expert_labels, student_labels))

# Define neural network
mlp = cv2.ml.ANN_MLP_create()
mlp.setLayerSizes(np.array([5, 10, 1]))
mlp.setActivationFunction(cv2.ml.ANN_MLP_SIGMOID_SYM, 1, 1)
mlp.setTrainMethod(cv2.ml.ANN_MLP_BACKPROP, 0.1, 0.1)

# Train the network
train_dataset = cv2.ml.TrainData_create(train_data, cv2.ml.ROW_SAMPLE, train_labels)
mlp.train(train_dataset)

# Save the model
mlp.save("shooting_coach_model.xml")
print("Training complete. Model saved as shooting_coach_model.xml")

# Real-time evaluation
def evaluate_shot(angle_x, angle_y, angle_z, trigger_smoothness, trigger_pull_time):
    input_data = np.array([[angle_x, angle_y, angle_z, trigger_smoothness, trigger_pull_time]], dtype=np.float32)
    _, prediction = mlp.predict(input_data)
    predicted_score = prediction[0][0]

    feedback = []
    if abs(angle_x) > 3 or abs(angle_y) > 3 or abs(angle_z) > 3:
        feedback.append("Your aim was unstable; try to keep the gun steadier.")
    if trigger_smoothness > 1:
        feedback.append("Your trigger pull was rough; apply more gradual pressure.")
    if abs(trigger_pull_time - 1) > 0.3:
        feedback.append("Your trigger pull speed was off; aim for about one second.")

    return predicted_score, feedback

# Simulate a student shot
test_shot = {
    "angle_x": 5,
    "angle_y": -4,
    "angle_z": 3,
    "trigger_smoothness": 1.3,
    "trigger_pull_time": 0.6
}

score, feedback = evaluate_shot(**test_shot)
print(f"Predicted Shot Score: {score:.2f}")
print("Feedback:")
for f in feedback:
    print(f"- {f}")
```

***

### **How the Code Works**

1. **Data Generation**: Simulates expert and student shooting data.
2. **Neural Network**: 5 inputs, 10 hidden neurons, 1 output.
3. **Training**: Uses OpenCV's `cv2.ml.ANN_MLP`.
4. **Inference**: Predicts shot scores and provides feedback.

### **Sample Output**

```
Predicted Shot Score: 55.32
Feedback:
- Your aim was unstable; try to keep the gun steadier.
- Your trigger pull was rough; apply more gradual pressure.
- Your trigger pull speed was off; aim for about one second.
```

### **What Is This? Expert System or Not?**

This system is **not** a traditional expert system. Here's why:

* **Expert System**:
  * Relies on predefined rules crafted by human experts.
  * Uses rule-based inference engines with hardcoded IF-THEN logic.
  * No learning from data.

* **This System**:
  * Learns patterns from expert and student data using machine learning.
  * Uses a neural network to generalize from examples.
  * Provides dynamic feedback based on data patterns rather than static rules.

AI researchers probably would call Auto-Coach  something like a **sensor-driven, supervised machine learning model** used for **performance assessment and feedback**.

Try saying THAT sentence fast... :)

***

### **C++ Code Example**

Here's a simple C++ implementation of the same idea also using OpenCV

```cpp
#include <opencv2/opencv.hpp>
#include <opencv2/ml.hpp>
#include <iostream>

using namespace cv;
using namespace cv::ml;

int main() {
    // Prepare training data
    Mat trainData = (Mat_<float>(6,5) <<
        0,0,0,0.5,1,
        1,1,1,0.4,0.9,
        2,2,2,0.6,1.1,
        3,3,3,1.5,0.5,
        4,4,4,2.0,0.4,
        5,5,5,1.2,0.7);

    Mat trainLabels = (Mat_<float>(6,1) << 100, 90, 80, 50, 40, 60);

    // Define the neural network
    Ptr<ANN_MLP> mlp = ANN_MLP::create();
    Mat layerSizes = (Mat_<int>(1,3) << 5, 10, 1);
    mlp->setLayerSizes(layerSizes);
    mlp->setActivationFunction(ANN_MLP::SIGMOID_SYM, 1, 1);
    mlp->setTrainMethod(ANN_MLP::BACKPROP, 0.1, 0.1);

    // Train the network
    Ptr<TrainData> trainingData = TrainData::create(trainData, ROW_SAMPLE, trainLabels);
    mlp->train(trainingData);

    // Test with a sample input
    Mat sample = (Mat_<float>(1,5) << 3,3,3,1.5,0.5);
    Mat result;
    mlp->predict(sample, result);

    std::cout << "Predicted Score: " << result.at<float>(0,0) << std::endl;

    return 0;
}
```
