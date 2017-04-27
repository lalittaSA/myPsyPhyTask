%% prepare to run AudiDeci_prior_HL task.
clear
close all;

topsDataLog.flushAllData();

%% paths & stuffs

data_folder = '/Research/uPenn_auditoryDecision/data/psychophysics/';

run_calibOnly = 0;
use_previousCalib = 2; % 0 - start new calibration | 1 - run calib & combine all previous calibration | 2 - use existing calibration files
%% calib task
[task_calib list_calib] = AudiDeci_noise_embedded_HL_ampCalib(1);

% get metadata
save_filename = list_calib{'meta'}{'saveFilename'};
meta_data_calib.subject = list_calib{'meta'}{'subjID'};
meta_data_calib.date = list_calib{'meta'}{'date'};
meta_data_calib.task = list_calib{'meta'}{'task'};
calib_folder = [data_folder meta_data_calib.task '/'];

% calib option
questVersion = 1;
%%
if use_previousCalib < 2
    % run calibration
    task_calib.run();
    
    hd = list_calib{'Stimulus'}{'header'};
    meta_data_calib.loFreq = hd.loFreq;
    meta_data_calib.hiFreq = hd.hiFreq;
    meta_data_calib.toneDur = hd.toneDur;
    meta_data_calib.toneIBI = hd.toneIBI;
    meta_data_calib.fs = hd.fs;
    nTrials = list_calib{'Counter'}{'trial'};
    meta_data_calib.nTrials = nTrials;
    meta_data_calib.questVersion = list_calib{'meta'}{'questVersion'};
    
    % trial data
    trialVarSequence = list_calib{'Control'}{'trialVarSequence'};
    isH = list_calib{'Stimulus'}{'isH'};
    testAmp = list_calib{'Stimulus'}{'testAmps'};
    success = list_calib{'Input'}{'corrects'};
    choice = list_calib{'Input'}{'choices'};
    rt = list_calib{'Input'}{'RT'};
    
    data_table_calib = table((1:nTrials)',trialVarSequence,isH,testAmp,success,choice,rt,'VariableNames',{'trialID','freq','isH','amplitude','success','choice','RT'});
    
    % save individual data
    save([calib_folder save_filename '_list.mat'], 'list_calib');
    save([calib_folder save_filename '_table.mat'], 'data_table_calib', 'meta_data_calib'); %Secondary, redundant save
end

    [subjAmpRange] = getDiscrimThreshold(calib_folder,questVersion,meta_data_calib.subject);

%%  main task

if ~run_calibOnly
    [task_main list_main] = AudiDeci_noise_embedded_HL_train(1,subjAmpRange,meta_data_calib.subject);
%     [task_main list_main] = AudiDeci_noise_embedded_HL(1,subjAmpRange,meta_data_calib.subject);
    
    task_main.run();
    
    %create data table
    save_filename = list_main{'meta'}{'saveFilename'};
    meta_data_main.subject = list_main{'meta'}{'subjID'};
    meta_data_main.date = list_main{'meta'}{'date'};
    meta_data_main.task = list_main{'meta'}{'task'};
    meta_data_main.ampRange = subjAmpRange;
    
    hd = list_main{'Stimulus'}{'header'};
    meta_data_main.loFreq = hd.loFreq;
    meta_data_main.hiFreq = hd.hiFreq;
    meta_data_main.toneDur = hd.toneDur;
    meta_data_main.toneIBI = hd.toneIBI;
    meta_data_main.fs = hd.fs;
    nTrials = list_main{'Counter'}{'trial'};
    meta_data_main.nTrials = nTrials;
    meta_data_main.sequenceLength = list_main{'Control'}{'sequenceLength'};
    meta_data_main.responseWindow = list_main{'Input'}{'responseWindow'};
    
    %trial data
    trialVarPreStim = list_main{'Control'}{'trialVarPreStim'};
    trialVarSequence = list_main{'Control'}{'trialVarSequence'};
    trialVarPrior = list_main{'Control'}{'trialVarPrior'};
    trialVarSNR = list_main{'Control'}{'trialVarSNR'};
    trialVarAmp = list_main{'Control'}{'trialVarAmp'};
    
    isH = list_main{'Stimulus'}{'isH'};
    waveforms = list_main{'Stimulus'}{'waveforms'};
    
    stimStart = list_main{'Timestamps'}{'stim_start'};
    stimStop = list_main{'Timestamps'}{'stim_stop'};
    responseTimeStamp = list_main{'Timestamps'}{'choices'};
    choices = list_main{'Input'}{'choices'};
    success = list_main{'Input'}{'corrects'};
    rt = list_main{'Input'}{'RT'};
    
    
    data_table_main = table((1:nTrials)',trialVarSequence,trialVarPreStim,trialVarPrior,trialVarSNR,trialVarAmp,isH,waveforms,stimStart,stimStop,responseTimeStamp,choices,success,rt,...
        'VariableNames',{'trialID','stimSequence','preToneSequence','prior','SNR','stimAmplitude','isH','waveform','stimStartTime','stimStopTime','responseTimeStamps','choice','success','RT'});
    
    
    %save individual data
    save([data_folder meta_data_main.task '/' save_filename '_list.mat'], 'list_main');
    save([data_folder meta_data_main.task '/' save_filename '_table.mat'], 'data_table_main', 'meta_data_main'); %Secondary, redundant save
    
end

clear
close all;

