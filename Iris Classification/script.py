import numpy as np  
import matplotlib.pyplot as plt  
import pandas as pd 
from sklearn.datasets import load_iris
from sklearn import metrics
from sklearn.neighbors import KNeighborsClassifier  
from sklearn.metrics import classification_report, confusion_matrix  
iris = load_iris()
iris.data
print(type(iris))
print("\n")

print("features")
print(iris.feature_names)

print("\ntarget")
print(iris.target)
print("\n")
print(iris.target_names)

print(iris.data.shape)

x_index = 0
y_index = 1

formatter = plt.FuncFormatter(lambda i, *args: iris.target_names[int(i)])

plt.figure(figsize=(5, 4))
plt.scatter(iris.data[:, x_index], iris.data[:, y_index], c=iris.target)
plt.colorbar(ticks=[0, 1, 2], format=formatter)
plt.xlabel(iris.feature_names[x_index])
plt.ylabel(iris.feature_names[y_index])

plt.tight_layout()
plt.show()