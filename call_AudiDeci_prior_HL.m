%% prepare to run AudiDeci_prior_HL task.
clear
close all;

topsDataLog.flushAllData();

% input = number of trials (per conditions)
[task list] = AudiDeci_prior_HL(0);

% visualize the task's structure
% tree.gui();
% list.gui();

% Run the task by invoking run() on the top-level object
% commandwindow();
% dotsTheScreen.openWindow()
task.run();
% dotsTheScreen.closeWindow();
%topsDataLog.gui();

%% Post-Processing
data_folder = '/Research/uPenn_auditoryDecision/data/psychophysics/';
save_filename = list{'Subject'}{'save_filename'};

% create data table
meta_data.subject = subj_id;
meta_data.date = datestr(now,'yymmdd');

hd = list{'Stimulus'}{'header'};
meta_data.loFreq = hd.loFreq;
meta_data.hiFreq = hd.hiFreq;
meta_data.toneDur = hd.toneDur;
meta_data.toneSOA = hd.toneSOA;
meta_data.trialDur = hd.trialDur;
meta_data.fs = hd.fs;

meta_data.nTrials = list{'Counter'}{'trial'};


% trial data
priorLevels = list{'control'}{'priorLevels'};
cohLevels = list{'control'}{'cohLevels'};
waveforms = list{'Stimulus'}{'waveforms'};
isH = list{'Stimulus'}{'isH'};
stimStart = list{'Timestamps'}{'stim_start'};
stimStop = list{'Timestamps'}{'stim_stop'};
responseTimeStamp = list{'Timestamps'}{'choices'};
choices = list{'Input'}{'choices'};
success = list{'Input'}{'corrects'};
rt = list{'Input'}{'RT'};

data_table = datatable((1:nTrials)',priorLevels,cohLevels,waveforms,isH,choices,success,stimStart,stimStop,responseTimeStamp,rt,'VariableNames',{'trialID','priorLevel','cohLevel','waveform','isH','choice','success','stimStartTime','stimStopTime','responseTimeStamps','RT'});

%% Saving
save([data_folder save_filename '_list.mat'], 'list');
save([data_folder save_filename '_table.mat'], 'data_table', 'meta_data'); %Secondary, redundant save