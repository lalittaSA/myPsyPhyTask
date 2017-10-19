clear
close all;

topsDataLog.flushAllData();

%% paths & stuffs

% data_folder = '/Research/uPenn_auditoryDecision/data/psychophysics/testTime/'; 
data_folder = './data/testTime/'; 

Screen('Preference','SkipSyncTests', 0);
[subjID, EDFfilename] = MKEyelinkCalibrate();

%% test1 - new sample
nTrials = 100000;

testNum = 9;
testGetSecs = 1;
testTicToc = 0;
testMessage = 0;

if ~testMessage
    timestampsEyeLink = zeros(nTrials,1);
    timeLapse = ones(nTrials,1);
end


if testGetSecs
    GetSecs;
    timestampsGetSecs = zeros(nTrials,1);
end

if testTicToc
    maintime = tic;
    timestampsTicToc = zeros(nTrials,1);
end

testBoth = 0;
if testGetSecs && testTicToc
    testBoth = 1;
end


for counter = 1:nTrials
    if testMessage
        Eyelink('Message', ['Trial_' num2str(counter)]);
    else
        while timeLapse(counter) > 1.5e-5
            lapse = tic;
            newsample = Eyelink('NewestFloatSample');
            timestampsEyeLink(counter) = newsample.time;
            timeLapse(counter) = toc(lapse);
        end
    end
    if testTicToc
        timestampsTicToc(counter) = toc(maintime);
    end
    if testGetSecs
        timestampsGetSecs(counter) = mglGetSecs;
    end
end
if testMessage
    if testBoth
        save([data_folder EDFfilename '_data.mat'], 'timestampsGetSecs','timestampsTicToc')
    else
        if testGetSecs
            save([data_folder EDFfilename '_data.mat'], 'timestampsGetSecs')
        elseif testTicToc
            save([data_folder EDFfilename '_data.mat'], 'timestampsTicToc')
        end
    end
else
if testBoth
    h = figure;
    diffGSEL = ((timestampsGetSecs*1000)-timestampsEyeLink) -  ((timestampsGetSecs(1)*1000)-timestampsEyeLink(1));
    diffTTEL = ((timestampsTicToc*1000)-timestampsEyeLink) -  ((timestampsTicToc(1)*1000)-timestampsEyeLink(1));
    plot(cumsum(diffGSEL))
    hold on
    plot(cumsum(diffTTEL))
    legend({'GetSecs - EyeLink', 'TicToc - EyeLink'})
    ylabel('time lag (ms)')
    xlabel('n iterations')
    fig_name = ['testTime' num2str(testNum)];
    title(fig_name)
    saveas(h,[data_folder fig_name '.fig']);
    close gcf
    save([data_folder EDFfilename '_data.mat'], 'timestampsGetSecs','timestampsTicToc', 'timestampsEyeLink','timeLapse')
else
    if testGetSecs
        h = figure;
        diffGSEL = ((timestampsGetSecs*1000)-timestampsEyeLink) -  ((timestampsGetSecs(1)*1000)-timestampsEyeLink(1));
        plot(cumsum(diffGSEL))
        legend({'GetSecs - EyeLink'})
        ylabel('time lag (ms)')
        xlabel('n iterations')
        fig_name = ['testGetSecsOnly'  num2str(testNum)];
        title(fig_name)
        saveas(h,[data_folder fig_name '.fig']);
        close gcf
        save([data_folder EDFfilename '_data.mat'], 'timestampsGetSecs', 'timestampsEyeLink','timeLapse')
    end
    if testTicToc
        h = figure;
        diffTTEL = ((timestampsTicToc*1000)-timestampsEyeLink) -  ((timestampsTicToc(1)*1000)-timestampsEyeLink(1));
        plot(cumsum(diffTTEL))
        legend({'TicToc - EyeLink'})
        ylabel('time lag (ms)')
        xlabel('n iterations')
        fig_name = ['testTicTocOnly'  num2str(testNum)];
        title(fig_name)
        saveas(h,[data_folder fig_name '.fig']);
        close gcf
        save([data_folder EDFfilename '_data.mat'], 'timestampsTicToc', 'timestampsEyeLink','timeLapse')
    end
end
end
%% test2 - tictoc & GetSecs

% tic;
% x = ones(1,100000);
% for n = 1:100000
%     x(n) = mglGetSecs;
% end
% z = ones(1,100000);
% for n = 1:100000
%     z(n) = toc;
% end

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
edfdata = edfmex(EDFfilename);

save([data_folder EDFfilename '_edf.mat'], 'edfdata')

clear
close all;

