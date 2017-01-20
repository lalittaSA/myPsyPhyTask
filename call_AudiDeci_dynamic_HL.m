%% prepare to run AudiDeci_dynamic_HL task.
clear
close all;

topsDataLog.flushAllData();

% input = number of trials (per conditions)
[task list] = AudiDeci_dynamic_HL(10);

% visualize the task's structure
% tree.gui();
% list.gui();

%% Run task
dotsTheScreen.openWindow();
task.run
dotsTheScreen.closeWindow();

%% Post-Processing
mat_folder = '/dataAnalysis/data/uPenn/psychophysics/mat/';
save_filename = [mfilename '_' subj_id '_' datestr(now,'yymmdd_HHMM')];

savename = list{'Subject'}{'ID'};
save(savename, 'list');


Data.Playtimes = list{'Timestamps'}{'Stimulus'}*1e6; %in usecs

Data.Choices = list{'Input'}{'Choices'}; %Choices from user signifying left/right
Data.Choicetimes = list{'Timestamps'}{'Choices'}*1e6; %in usecs

Data.Metahazard = list{'Stimulus'}{'Metahazard'}; %The metahazard used to switch between hazard rates
Data.Hazards = list{'Stimulus'}{'Hazards'}; %The set of hazard rates used
Data.Statelist = list{'Stimulus'}{'Statelist'}; %Which hazard rate was being used during which trial
Data.Dists = list{'Stimulus'}{'Dists'}; %Set of probability distributions used
Data.Distlist = list{'Stimulus'}{'Distlist'}; %1 means first distribution, 2 means second
Data.Directionlist = list{'Stimulus'}{'Directionlist'}; %1 means left, 2 means right

save(['Data_' savename], 'Data'); %Secondary, redundant save



Data.StandardFreq = list{'Stimulus'}{'StandardFreq'};
Data.OddFreq = list{'Stimulus'}{'OddFreq'};
Data.ProbabilityOdd = list{'Stimulus'}{'ProbabilityOdd'};
Data.ResponsePattern = list{'Input'}{'ResponsePattern'}; % buttons to press 
Data.MotorEffort = list{'Input'}{'Effort'};
Data.StimTimestampsOne = list{'Stimulus'}{'Playtimes'}; %Store sound player timestamps (in seconds)
Data.StimTimestampsTwo = list{'Stimulus'}{'PlaytimesTwo'};
Data.EyelinkTimestamps = list{'Eyelink'}{'Timestamps'};  %Store eyelink timestamps (in milliseconds)
Data.EyelinkInterval = list{'Eyelink'}{'Interval'}; % Delay to get the timestamps of the eyetracker (in seconds)
Data.StimFrequencies = list{'Stimulus'}{'Playfreqs'};% Store whether the trial triggered a standard sound or an oddball
Data.Choices = list{'Input'}{'Choices'}; %Storing if subject pressed the buttons required (if they press another set of buttons, value=0)>
Data.Corrects = list{'Input'}{'Corrects'}; %Storing correctness of answers (1= true, 0=false). Initialized to 33 so we know if there was no input during a trial with 33.
Data.ChoiceTimestamps = list{'Timestamps'}{'Response'}; %Storing subject response timestamp

%% Saving

save([list{'Subject'}{'Savename'} '.mat'],'list')
save([ list{'Subject'}{'Savename'} '_Data' '.mat'], 'Data') 