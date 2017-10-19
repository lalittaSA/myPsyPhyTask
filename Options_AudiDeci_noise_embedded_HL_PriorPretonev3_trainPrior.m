function options = Options_AudiDeci_noise_embedded_HL_PriorPretonev3_trainPrior(options)
%% pretone settings
options.nPreTones = 0;
options.preToneCombination = 'single'; % select from {'single','combine','permute'};

%% prior & amplitude settings
options.priorLevels = [-1 0 1]; % from {-3 -2 -1 0 1 2 3} | prior setting (arbitrary numbers -> used to set location of pre-cue)
% amplitude setting : minus - low | plus - high -> depending on prior : positive prior -> more positive snr trials
% options.snrLevels = [-0.6 -0.2 -0.05 -0.01 0.01 0.05 0.2 0.6]; % must be symmetric
options.snrLevels = [-1 -0.4 -0.1 0.1 0.4 1];

options.snrPrior = [3 2 1; ... % low-freq trials
                    1 2 3];    % high-freq trials

%% number of repetitions (total trials = nRep * length(nPreTones) * sum(snrPrior))
options.nRep = 1;

%% response
options.responsewindow = 1; %Time allowed to respond in sec