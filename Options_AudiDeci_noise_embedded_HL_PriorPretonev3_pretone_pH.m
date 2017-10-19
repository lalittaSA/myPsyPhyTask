function options = Options_AudiDeci_noise_embedded_HL_PriorPretonev3_pretone_pH(options)
%% pretone settings
% options.nPreTones = [0 3 5 6 7 8 9 10];
options.nPreTones = [0 2:14];
options.preToneCombination = 'single'; % select from {'single','combine','permute'};


%% prior & amplitude settings
options.priorLevels = [2]; % from {-3 -2 -1 0 1 2 3} | prior setting (arbitrary numbers -> used to set location of pre-cue)
% amplitude setting : minus - low | plus - high -> depending on prior : positive prior -> more positive snr trials  
options.snrLevels = [-1 -0.4 -0.2 -0.1 0.1 0.2 0.4 1]; % must be symmetric

options.snrPrior = [1; ... % low-freq trials
                    5];    % high-freq trials

%% number of repetitions (total trials = nRep * sum(snrPrior)) * preToneBlockSize(= nSeqBias (4) * nSeqLength(4))
options.nRep = 2;

%% response
options.responsewindow = 2; %Time allowed to respond in sec