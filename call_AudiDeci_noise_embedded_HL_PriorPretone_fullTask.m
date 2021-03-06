%% prepare to run AudiDeci_prior_HL task.
clear
close all;

topsDataLog.flushAllData();

%% test gamepad

[task_game list_game] = gamepadTest(1);
task_game.run();

clear task_game list_game

%% paths & stuffs

data_folder = './data/';
% data_folder = '/Research/uPenn_auditoryDecision/data/psychophysics/';

run_taskWithoutToneCalib = 1;
maxSNR = 0.5;  % in case of no calibration - set maximal dB for both H-L

eyeTrackerOn = 1;

trainOnly = 0;
session = 1; % 1/2/3/4

options = Options_AudiDeci_noise_embedded_HL_PriorPretonev2_main;

%% calib task

% eye calib
if run_taskWithoutToneCalib
    if eyeTrackerOn
        Screen('Preference','SkipSyncTests', 0);
        [subjID, EDFfilename] = MKEyelinkCalibrate();
    else
        subjID = input('Subject ID is: ', 's');
    end
    
    calibFileName = 'calibration_current.mat';
    
    if exist(calibFileName,'file')
        load(calibFileName)
        % check whether frequencies
        if ismember(250,calibrationFile.cf) && ismember(2000,calibrationFile.cf)
            dB_list = calibrationFile.targetLevels; % becomes calibrationFile.targetLevels after new calibration
            voltageLo = calibrationFile.calibratedamplitude(:,ismember(calibrationFile.cf,250));
            voltageHi = calibrationFile.calibratedamplitude(:,ismember(calibrationFile.cf,2000));
        else
            error('Error: Low or high frequencies selected are not calibrated!')
        end
    else
        error('Error: calibration file not found!')
    end

    options.ampRanges = [0 voltageLo(dB_list == options.noiseAmp); 0 voltageHi(dB_list == options.noiseAmp)];
    options.ampRanges = options.ampRanges * options.maxSNR;
else
    nCalib = 1;
    use_previousCalib = 1; % 0 - recalibrate | 1 - use existing calibration data of the subject
    
    if eyeTrackerOn
        Screen('Preference','SkipSyncTests', 0);
        [subjID, EDFfilename] = MKEyelinkCalibrate();
        [task_calib list_calib] = AudiDeci_noise_embedded_HL_ampCalib_dB(1,subjID,1);
    else
%         [task_calib list_calib] = AudiDeci_noise_embedded_HL_ampCalib(1);
        [task_calib list_calib] = AudiDeci_noise_embedded_HL_ampCalib_dB(1);
        subjID = list_calib{'meta'}{'subjID'};
    end
    
    %%
    meta_data_calib.subject = list_calib{'meta'}{'subjID'};
    meta_data_calib.date = list_calib{'meta'}{'date'};
    meta_data_calib.task = list_calib{'meta'}{'task'};
    questVersion = list_calib{'meta'}{'questVersion'};
    
    calib_folder = [data_folder meta_data_calib.task '/'];
    
    if ~use_previousCalib
        for ii = 1:nCalib
            if ii > 1
                [task_calib, list_calib] = AudiDeci_noise_embedded_HL_ampCalib_dB(1,subjID);
            end
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
            
            % save calib list into table
%                         data_table_calib = table((1:nTrials)',trialVarSequence(1:nTrials),isH(1:nTrials),testAmp(1:nTrials),success(1:nTrials),choice(1:nTrials),rt(1:nTrials),'VariableNames',{'trialID','freq','isH','amplitude','success','choice','RT'});

            data_table_calib = table((1:nTrials)',trialVarSequence,isH,testAmp,success,choice,rt,'VariableNames',{'trialID','freq','isH','amplitude','success','choice','RT'});
            
            save_filename = [list_calib{'meta'}{'saveFilename'} '_' num2str(ii)];
            % save individual data
            save([calib_folder save_filename '_list.mat'], 'list_calib');
            save([calib_folder save_filename '_table.mat'], 'data_table_calib', 'meta_data_calib'); %Secondary, redundant save
            clear task_calib list_calib
        end
    end
%     [subjAmpRange] = getDiscrimThreshold(calib_folder,questVersion,subjID);
    [options.ampRanges] = getDiscrimThreshold_dB(calib_folder,questVersion,subjID);
    options.ampRanges(:,1) = 0;
end
%%  main task

if trainOnly
    switch session
        case 1
            options.optionName = 'trainPrior';
        case 2
            options.optionName = 'trainPretone';
        case 3
            options.optionName = 'trainPretoneL';
        case 4
            options.optionName = 'trainPretoneH';
    end
    
%     [task_main list_main] = AudiDeci_noise_embedded_HL_PriorPretonev2_train(1,subjID,options,eyeTrackerOn);
    [task_main list_main] = AudiDeci_noise_embedded_HL_PriorPretonev3(1,subjID,options,eyeTrackerOn);

else
    switch session
        case 1
            options.optionName = 'priorOnly';
        case 2
            options.optionName = 'pretoneOnly';
        case 3
            options.optionName = 'pretone_pL';
        case 4
            options.optionName = 'pretone_pH';
    end
%     [task_main list_main] = AudiDeci_noise_embedded_HL_PriorPretonev2(1,subjID,options,eyeTrackerOn);
    [task_main list_main] = AudiDeci_noise_embedded_HL_PriorPretonev3(1,subjID,options,eyeTrackerOn);

end
task_main.run();

%create data table
save_filename = list_main{'meta'}{'saveFilename'};
meta_data_main.subject = list_main{'meta'}{'subjID'};
meta_data_main.date = list_main{'meta'}{'date'};
meta_data_main.task = list_main{'meta'}{'task'};
meta_data_main.optionName = list_main{'meta'}{'optionName'};
meta_data_main.ampRange = options.ampRanges;

meta_data_main.loFreq = list_main{'Stimulus'}{'loFreq'};
meta_data_main.hiFreq = list_main{'Stimulus'}{'hiFreq'};
meta_data_main.toneDur = list_main{'Stimulus'}{'toneDur'};
meta_data_main.toneIBI = list_main{'Stimulus'}{'toneIBI'};
meta_data_main.fs = list_main{'Stimulus'}{'fs'};
nTrials = list_main{'Counter'}{'nTrials'};
meta_data_main.nTrials = nTrials;
meta_data_main.responseWindow = list_main{'Timing'}{'responseWindow'};

%trial data
trialVarAmp = list_main{'Control'}{'trialVarAmp'};
trialVarSNR = list_main{'Control'}{'trialVarSNR'} * options.maxSNR;
trialVarPrior = list_main{'Control'}{'trialVarPrior'};
trialVarStimSeq = list_main{'Control'}{'trialVarStimSeq'};
trialVarPreToneSeq = list_main{'Control'}{'trialVarPreToneSeq'};
trialVarPreToneBias = list_main{'Control'}{'trialVarPreToneBias'};
trialVarPreToneLength = list_main{'Control'}{'trialVarPreToneLength'};
trialVarLastConsPretone = list_main{'Control'}{'trialVarLastConsPretone'};

isH = list_main{'Stimulus'}{'isH'};

%timestamps
trialStarts = list_main{'Timestamps'}{'trialStarts'};
trialStops = list_main{'Timestamps'}{'trialStops'};
stimStarts = list_main{'Timestamps'}{'stimStarts'};
stimStops = list_main{'Timestamps'}{'stimStops'};

choiceTimeStamp = list_main{'Timestamps'}{'choices'};

rtOffset = list_main{'Timestamps'}{'rtOffset'};

rt = list_main{'Input'}{'RT'};
choices = list_main{'Input'}{'choices'};
success = list_main{'Input'}{'corrects'};

data_table_main = table((1:nTrials)',trialVarAmp,trialVarSNR,trialVarPrior,trialVarStimSeq,trialVarPreToneSeq,trialVarPreToneBias,trialVarPreToneLength,trialVarLastConsPretone,isH,...
    success,choices,rt,trialStarts,trialStops,stimStarts,stimStops,choiceTimeStamp,rtOffset,...
    'VariableNames',{'trialID','stimAmplitude','SNR','prior','stimSequence','pretoneSeq','pretoneBias','pretoneLength','LastConsPretone','isH',...
    'success','choice','RT','trialStarts','trialStops','stimStarts','stimStops','choiceTimeStamp','RToffset'});
%     data_table_main = table((1:nTrials)',trialVarAmp,trialVarSNR,trialVarPrior,trialVarStimSeq,trialVarPreToneBias,trialVarPreToneLength,isH,...
%         success,choices,rt,trialStarts,trialStops,stimStarts,stimStops,choiceTimeStamp,rtOffset,...
%         'VariableNames',{'trialID','stimAmplitude','SNR','prior','stimSequence','pretoneBias','pretoneLength','isH',...
%         'success','choice','RT','trialStarts','trialStops','stimStarts','stimStops','choiceTimeStamp','RToffset'});


if eyeTrackerOn
    stimOn_el = list_main{'Eyelink'}{'stimOnTimestamps'};
    preStimOn = list_main{'Eyelink'}{'preStimOnTimestamps'};
    postStimOn = list_main{'Eyelink'}{'postStimOnTimestamps'};
    response_el = list_main{'Eyelink'}{'responseTimestamps'};
    postResponse = list_main{'Eyelink'}{'postResponseTimestamps'};
    trialStop_el = list_main{'Eyelink'}{'trialStopTimestamps'};
    trialStart_el = list_main{'Eyelink'}{'trialStartTimestamps'};
    data_table_main = table((1:nTrials)',trialVarAmp,trialVarSNR,trialVarPrior,trialVarStimSeq,trialVarPreToneSeq,trialVarPreToneBias,trialVarPreToneLength,trialVarLastConsPretone,isH,...
        success,choices,rt,trialStarts,trialStops,stimStarts,stimStops,choiceTimeStamp,rtOffset,...
        trialStart_el,preStimOn,stimOn_el,postStimOn,response_el,postResponse,trialStop_el,...
        'VariableNames',{'trialID','stimAmplitude','SNR','prior','stimSequence','pretoneSeq','pretoneBias','pretoneLength','LastConsPretone','isH',...
        'success','choice','RT','trialStarts','trialStops','stimStarts','stimStops','choiceTimeStamp','RToffset',...
        'trialStart_el','preStimOn','stimOn_el','postStimOn','response_el','postResponse','trialStop_el'});
end

%save individual data
save([data_folder meta_data_main.task '/' save_filename '_list.mat'], 'list_main');
save([data_folder meta_data_main.task '/' save_filename '_table.mat'], 'data_table_main', 'meta_data_main'); %Secondary, redundant save



%% Saving Eyelink Data 
%Close file, stop recording
Eyelink('StopRecording');
Eyelink('Command','set_idle_mode');
WaitSecs(0.5);
Priority();
Eyelink('CloseFile');

cd([data_folder meta_data_main.task '/'])
try
    fprintf('Receiving data file ''%s''\n', EDFfilename);
    status = Eyelink('ReceiveFile', EDFfilename);
    if status > 0
        fprintf('ReceiveFile status %d\n', status);
    end
    if 2 == exist(EDFfilename, 'file')
        fprintf('Data file ''%s'' can be found in ''%s''\n', EDFfilename, pwd );
    end
catch rdf
    fprintf('Problem receiving data file ''%s''\n', EDFfilename );
    rdf;
end

% Convert edf file in mat file
edfdata = edfmex(EDFfilename,'.edf');

save([save_filename '_edf.mat'], 'edfdata')

clear
close all;

cd ..
cd ..


