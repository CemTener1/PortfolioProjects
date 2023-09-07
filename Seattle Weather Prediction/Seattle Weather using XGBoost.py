#!/usr/bin/env python
# coding: utf-8

# In[28]:


#import necessary libraries
import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix, accuracy_score
from sklearn.preprocessing import LabelEncoder
import matplotlib.pyplot as plt


# In[14]:


mydata = pd.read_csv('/Users/newyorker/Desktop/Data Science Portfolio/Seattle Weather Prediction/seattle-weather.csv')
mydata.head(10)
#select necessary columns
mydata = mydata[['precipitation', 'temp_max', 'temp_min', 'wind', 'weather']]
pd.factorize('weather')


# In[25]:


np.random.seed(2000)
train_df, valid_df = train_test_split(mydata, test_size=0.2)

X_train = train_df.drop('weather', axis = 1)
Y_train = train_df['weather']
X_test = valid_df.drop('weather', axis = 1)
Y_test = valid_df['weather']

label_encoder = LabelEncoder()
Y_train = label_encoder.fit_transform(Y_train)
Y_test = label_encoder.transform(Y_test)


# In[49]:


# fit the model
model = xgb.XGBClassifier(max_depth = 5, seed = 2000)
model.fit(X_train, Y_train)


# In[50]:


# make predictions
model_pred = model.predict(X_test)
#Create confusion matrix
confusion = confusion_matrix(Y_test, model_pred)
accuracy = accuracy_score(Y_test, model_pred)
print(confusion)
print("Accuracy:", accuracy)


# In[46]:


#Create a deeper tree
deeper_model = xgb.XGBClassifier(max_depth = 100, seed = 2000)
deeper_model.fit(X_train, Y_train)


# In[48]:


#Make predictions
deeper_pred = deeper_model.predict(X_test)

confusion_deeper = confusion_matrix(Y_test, deeper_pred)
accuracy_deeper = accuracy_score(Y_test, deeper_pred)
print(confusion_deeper)
print("Accuracy (Deeper Tree):", accuracy_deeper)

