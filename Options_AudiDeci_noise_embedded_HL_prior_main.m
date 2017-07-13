function options = Options_AudiDeci_noise_embedded_HL_prior_main
%% frequency settings
options.freqTypes = [1 2];
options.freqNames = {'L';'H'};

options.loFreq = 500; %hz      312.5 |  625 | 1250 | 2500 |  5000
options.hiFreq = 2000; %hz     625   | 1250 | 2500 | 5000 | 10000 
options.toneDur = 300; %ms 25 | 50
options.toneIBI = 100; %ms  5 | 10

options.fs = 44100;%384000;

%% pretone settings
options.nPreTones = 0;
options.preToneFreqs = 'single'; % select from {'single','combine','permute'};


%% prior & amplitude settings
options.priorLevels = [-2 0 2]; % from {-3 -2 -1 0 1 2 3} | prior setting (arbitrary numbers -> used to set location of pre-cue)
% amplitude setting : minus - low | plus - high -> depending on prior : positive prior -> more positive snr trials
options.snrLevels = [-0.7 -0.3 -0.1 -0.05 -0.01 0.01 0.05 0.1 0.3 0.7];  % must be symmetric
% options.snrLevels = [-1 -0.6 -0.3 -0.1 -0.01 0.01 0.1 0.3 0.6 1];

options.snrPrior = [5 3 1; ... % low-freq trials
                    1 3 5];    % high-freq trials

%% number of repetitions (total trials = nRep * length(nPreTones) * sum(snrPrior))
options.nRep = 6;

%% response
options.responsewindow = 2; %Time allowed to respond in sec