clear
clc
close all
s11= readmatrix('pressureSensorData_3a1.csv');
s12 = readmatrix('pressureSensorData_3a2.csv');
s21 = readmatrix('pressureSensorData_3b1.csv');
s22 = readmatrix('pressureSensorData_3b2.csv');
s31 = readmatrix('pressureSensorData_3c1.csv');
s32 = readmatrix('pressureSensorData_3c2.csv');
s4a = readmatrix('pressureSensorData_4a.csv');
s4b = readmatrix('pressureSensorData_4b.csv');
s4c = readmatrix('pressureSensorData_4c.csv');
s5 = readmatrix('Luyitian_Respiration.csv');

%% Figure 1 (part 4)
figure;
hold on;

data_all = {s11, s12, s21, s22, s31, s32};% easier for loop
labels = {'No Object Sample 1 R2=1.867 e+03', 'No Object Sample 2 R2=1.5142e+03','Object A Sample 1 R2 =205.1425', 'Object A Sample 2 R2=239.4368', 'Object B Sample 1 R2=566.6773', 'Object B Sample 2 R2=518.2648'};

for i = 1:length(data_all)
    current_s = data_all{i};
    plot(current_s(:,1), current_s(:,2), 'DisplayName', labels{i});
end

xlabel('Time (s)');
ylabel('Voltage (V)');
title('Figure 1: Voltage Output for Different Loads');
legend
hold off;


%% Figure 2 (part 5 )
figure;
hold on;

data_r1 = {s4a, s4b, s4c};% same as before , easier for loop
labels_r1 = {'Single Resistor', 'Two Resistors in Series', 'Two Resistors in Parallel'};

for j = 1:length(data_r1)
    current_data = data_r1{j};
    plot(current_data(:,1), current_data(:,2), 'DisplayName', labels_r1{j});
end

xlabel('Time (s)');
ylabel('Voltage (V)');
title('Figure 2: Effect of Changing R1 Configuration');
legend
ylim([0 5.5]); 
hold off;


%% Figure 3 (part 6)

figure;
plot(s5(:, 1), s5(:, 2));
xlabel('Time (s)');
ylabel('Voltage (V)');
title('Figure 3: 5-Minute Respiration Voltage Data');


 



