clear
close all;

topsDataLog.flushAllData();

%% paths & stuffs

data_folder = '/Research/uPenn_auditoryDecision/data/psychophysics/testTime/'; 

Screen('Preference','SkipSyncTests', 0);
[subjID, EDFfilename] = MKEyelinkCalibrate();

%% test1 - new sample

tic
trialStartTimestamps = list{'Eyelink'}{'trialStartTimestamps'};
newsample = Eyelink('NewestFloatSample');
trialStartTimestamps(counter) = newsample.time;
list{'Eyelink'}{'trialStartTimestamps'} = trialStartTimestamps;
toc

%% test2 - tictoc & GetSecs

tic;
x = ones(1,100000);
for n = 1:100000
    x(n) = mglGetSecs;
end
z = ones(1,100000);
for n = 1:100000
    z(n) = toc;
end

%% Saving Eyelink Data 
%Close file, stop recording
Eyelink('StopRecording');
Eyelink('Command','set_idle_mode');
WaitSecs(0.5);
Priority();
Eyelink('CloseFile');

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
if ~run_calibOnly
    save([data_folder meta_data_main.task '/' save_filename '_edf.mat'], 'edfdata')
else
    save([calib_folder save_filename '_edf.mat'], 'edfdata')
end
clear
close all;

