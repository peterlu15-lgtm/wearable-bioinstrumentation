clear 
clc
close all

%% Connect Arduino

a = arduino();

%% Collect and Save Data

% define variables and call pressureSensor function
sampleTime = 10; % As required the time that this code will record
thresh = 3.6;%If it goes below / equal 3.6V the LED blinks
livePlot = false;%If chose true, then there will be a lot more calculations since matlab needs to draw a new graph every sample
pauseTime = 0.1;%we can force the matlab to take a 'break'
[data] = pressureSensor(a,sampleTime,thresh,livePlot,pauseTime);



% calcuate data acqusition rate
fs = length(data.time) / data.time(end); 

R1 = 100; % R1 is brown,black brown 100 ohm   
Vin = 5; %power supply from the arduino 
Vout_avg = mean(data.voltage); % just to see the avg Vout

r2 = (Vout_avg * R1) / (Vin - Vout_avg);% refromat the voltage divider to have the r2

% save pressureSensor output table to a study array, table, or structure
study = data;

% save pressureSensor output table to a csv in your data folder
writetable(data,'C:\Users\peter\git\wearable-bioinstrumentation\data\pressureSensorData_4c.csv')

%% Figure 1. Calculate Resistance, Sry,I had the data first, the plotting will be in another .m file named

%figure
%plot(data.time,data.voltage)

%% Figure 2. Changing R1

%figure
%plot(data.time,data.voltage)

%% Figure 3. Respiration

%figure
%plot(data.time,data.voltage)
