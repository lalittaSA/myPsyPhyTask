function options = Options_AudiDeci_noise_embedded_HL_PriorPretonev2_main
%% frequency settings
options.freqTypes = [1 2];
options.freqNames = {'L';'H'};

options.loFreq = 250; %hz      312.5 |  625 | 1250 | 2500 |  5000
options.hiFreq = 2000; %hz     625   | 1250 | 2500 | 5000 | 10000 
options.toneDur = 300; %ms 25 | 50
options.toneIBI = 100; %ms  5 | 10

options.fs = 44100;%384000;

options.noiseAmp = 75;
options.maxSNR = 0.5;

options.goCueBefore = 1;

options.eyeFixCheck = 1;
options.eyeFixHold = 1;

options.pretoneFlip = 1;
options.pretoneFlipProb = 0.05;
