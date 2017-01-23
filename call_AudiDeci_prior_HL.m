%% prepare to run AudiDeci_prior_HL task.
clear
close all;

topsDataLog.flushAllData();

% input = number of trials (per conditions)
[task list] = AudiDeci_prior_HL(1);

% visualize the task's structure
% tree.gui();
% list.gui();

% Run the task by invoking run() on the top-level object
% commandwindow();
% dotsTheScreen.openWindow()
task.run();
% dotsTheScreen.closeWindow();
%topsDataLog.gui();

% Post-Processing
data_folder = '/Research/uPenn_auditoryDecision/data/psychophysics/';
save_filename = list{'meta'}{'save_filename'};

% create data table
meta_data.subject = list{'meta'}{'subjID'};
meta_data.date = list{'meta'}{'date'};

hd = list{'Stimulus'}{'header'};
meta_data.loFreq = hd.loFreq;
meta_data.hiFreq = hd.hiFreq;
meta_data.toneDur = hd.toneDur;
meta_data.toneSOA = hd.toneSOA;
meta_data.trialDur = hd.trialDur;
meta_data.fs = hd.fs;
nTrials = list{'Counter'}{'trial'};
meta_data.nTrials = nTrials;


% trial data
priorLevels = list{'control'}{'priorLevels'};
cohLevels = list{'control'}{'cohLevels'};
coh_played = list{'Stimulus'}{'coh_played'};

isH = list{'Stimulus'}{'isH'};
isH_played = list{'Stimulus'}{'isH_played'};

numTones_played = list{'Stimulus'}{'numTones_played'};
waveforms = list{'Stimulus'}{'waveforms'};

stimStart = list{'Timestamps'}{'stim_start'};
stimStop = list{'Timestamps'}{'stim_stop'};
responseTimeStamp = list{'Timestamps'}{'choices'};
choices = list{'Input'}{'choices'};
success = list{'Input'}{'corrects'};
rt = list{'Input'}{'RT'};

data_table = table((1:nTrials)',priorLevels,cohLevels,coh_played,numTones_played,waveforms,isH,isH_played,choices,success,stimStart,stimStop,responseTimeStamp,rt,'VariableNames',{'trialID','priorLevel','cohLevel','coh_played','numTones_played','waveform','isH','isH_played','choice','success','stimStartTime','stimStopTime','responseTimeStamps','RT'});

%% Saving
save([data_folder save_filename '_list.mat'], 'list');
save([data_folder save_filename '_table.mat'], 'data_table', 'meta_data'); %Secondary, redundant save