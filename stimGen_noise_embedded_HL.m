function [td,s,frequencyBuffer,isH] = stimGen_noise_embedded_HL(loFreq,hiFreq,freqType,toneDur,toneIBI,toneAmpL,toneAmpH,lastToneAmp,noiseAmp,fs);
% stimGen_noise_embedded_HL generates a sequence of tone bursts generated from 2 frequencies (loFreq & hiFreq)
% the first n-1 tones have clear signal to noise ratio while the last tone has varied SNR
% each tone lasts 'toneDur' ms and is followed by an inter-burst interval of 'toneIBI'
% the standard tone amplitude is determined by 'toneAmp' % fixed for each subject
% the noise amplitude is determined by 'noiseAmp' % fixed for each subject
% the amplitude of the last tone is determined by 'lastToneAmp' % trial variable
% the frequency pattern is given by 'freqType' % trial variable

% INPUT:
% loFreq    - lowest frequency in Hz
% hiFreq    - highest frequency in Hz
% toneDur   - tone duration in ms
% toneIBI   - stimulus onset asynchrony = inter-tone interval in ms
% trialDur  - trial duration in ms
% freqType  - frequency patterns [HHHL | LLLL | LLLH]
% toneAmpL  - amplitude of the low-frequency pre tones
% toneAmpH  - amplitude of the high-frequency pre tones
% lastToneAmp  - amplitude of the last (decesion) tone

% OUTPUT:
% td - time bins
% s - auditory stream

%% some default variable for function testing
test = 0;
if test
    loFreq = 500; %hz  625 | 1250 | 2500 | 5000
    hiFreq = 2000; %hz   1250 | 2500 | 5000 | 10000
    toneDur = 500; %ms
    toneIBI = 100; %ms
    freqType = 'H';
    lastToneAmp = 0.03;
    % toneAmpL = 0.6;
    % toneAmpH = 0.6;
    toneAmpL = 0.01;%calibrationFile.calibratedamplitude(1);
    toneAmpH = 0.001;%calibrationFile.calibratedamplitude(2);
    fs = 44100;%2e5;
    noiseAmp = 0.0118;
%     noiseAmp = 0;
end

%%
database_calib_filename = 'calibNoise.mat';
if exist(database_calib_filename,'file')
    load(database_calib_filename)
    invFilter = CalibData.hinv;
else
    error('noise calibration file not found!')
end
%% generate noise
% [B1,A1] = fir1(512,0.95);
% fvtool(B1,A1,'Fs',fs)

%% generate sequence of frequencies

rampLength = 10;
cosRamp = (rampLength/1000)*fs;
nTones = length(freqType);
trialDur = (toneDur*nTones) + (toneIBI*(nTones-1));
frequencyBuffer = zeros(nTones,1);
multiplier = floor(fs/1000);
s = zeros(1,trialDur*multiplier);
f = nan(1,trialDur*multiplier);
ind = 1;

for ii = 1:nTones
    tmax = toneDur/1000;                               % Time Duration Of Signal (sec)
    t = linspace(0, tmax, tmax*fs);                          % Time Vector
    
    switch freqType(ii)
        case 'H', tmpFreq = hiFreq; toneAmp = toneAmpH;      % high frequency
        case 'L', tmpFreq = loFreq; toneAmp = toneAmpL;      % low frequency
        case 'N', tmpFreq = loFreq; toneAmp = 0; lastToneAmp = 0;
    end
    
    % generate tone
    tone = sin(2*pi*tmpFreq*t);
    tone = pa_ramp(tone, cosRamp, fs);
    
    % generate noise
    white_noise = randn(fs*tmax, 1);
%     noise_limited = filter(B1,A1,white_noise);
    noise_limited = conv(white_noise,invFilter) * noiseAmp;
    
    if ii == nTones % if last tone
        tone = (lastToneAmp*tone) + noise_limited(1:length(tone));
    else
        tone = (toneAmp*tone) + noise_limited(1:length(tone));
    end
    
    frequencyBuffer(ii) = tmpFreq;
    s(ind:ind+length(tone)-1) = tone';
    f(ind:ind+length(tone)-1) = tmpFreq;
    if ii < nTones
        ind = ind+length(tone)+(toneIBI/1000*fs);
    else
        ind = ind+length(tone)-1;
        s = s(1:ind);
        f = f(1:ind);
    end
end
td = 0:1/fs:(length(s)-1)/fs;

if strcmp(freqType(nTones),'H') 
    isH = 1; 
else
    isH = 0;
end

%% for testing - listen
if test
 sound(s', fs) 
end
% noiser = dotsPlayableWave();
% noiser.wave = s;
% noiser.sampleFrequency = fs;
% % noiser.duration = 1;
% noiser.intensity = 0.5;
% noiser.prepareToPlay;
% noiser.play