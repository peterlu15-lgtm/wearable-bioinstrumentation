% analyzeRESP calculates respiration rate using time and frequency domain
% analyses

function [rr,rr_fft] = analyzeRESP(time,resp,plotsOn)
    % INPUTS: 
    % time: elapsed time (seconds)
    % resp: output from pressure sensor (voltage)
    % plotsOn: true for plots, false for no plots
    
    % OUTPUT:
    % rr: respiration rate (brpm) found from time domain data
    % rr_fft: respiration rate (brpm) found from frequency domain data

    % save orgiinal data
    time_raw = time;
    resp_raw = resp;

    % calculate fs
    fs = length(time) / (time(end) - time(1)); % FILL IN CODE HERE

    % remove offset
    resp = resp - mean(resp);

    % bandpass pass filter resp
    w1 = 0.1; % FILL IN CODE HERE
    w2 = 0.8; % FILL IN CODE HERE
    resp = bandpass(resp,[w1 w2],fs);

    % find peaks
    [pks, locs] = findpeaks(resp, time, 'MinPeakDistance', 1.2); % FILL IN CODE HERE (look at findpeaks documentation)

    % calcuate rr
    total_time_min = (time(end) - time(1)) / 60;
    rr = length(pks) / total_time_min; % FILL IN CODE HERE

    % fft
    L = length(resp);          
    Y = fft(resp);             
    P2 = abs(Y/L);              %
    P1 = P2(1:floor(L/2)+1);    % 
    P1(2:end-1) = 2*P1(2:end-1);
    f = fs*(0:(L/2))/L;         % 
    % calcuate rrFft
    [max_amp, max_idx] = max(P1);
    freq_max = f(max_idx);      %  Hz
    rr_fft = freq_max * 60;     %  brpm % FILL IN CODE HERE (hint: look at max documentation)

    if plotsOn
        figure % FILL IN CODE HERE to add legends, axes labels, and * for peaks
        subplot(3,1,1) 
        plot(time_raw,resp_raw)
        title('Raw Respiration Signal');
        ylabel('Voltage (V)');

        subplot(3,1,2)
        plot(time, resp); 
        hold on; 
        plot(locs, pks, 'r*'); 
        title(['Filtered Signal (RR: ', num2str(rr, '%.2f'), ' brpm)']);
        ylabel('Voltage (V)');
        legend('RESP', 'Peaks');
        hold off;

        subplot(3,1,3)
        plot(f,P1)
        hold on;
        plot(freq_max, max_amp, 'r*'); 
        title('')
        title('Amplitude Spectrum'); 
        ylabel('|P1(f)|');
        legend('RESP', 'Peaks');

        hold off;

    end
end