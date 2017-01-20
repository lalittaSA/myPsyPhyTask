function [task, list] = AudiDeci_dynamic_HL(nRep)

% 20170114: created by Lalitta - auditory decision task - 
% 2-alternative forced choice task: low-high frequency discrimination 
% stimulus-response combinations: high - left | low - right 
% using gamepade (L/R buttons) or keyboard (F/J keys; if a HID gamepad is not connected)

% Feedback is given by auditory signal (coin sound - correct, low-pitched beep - incorrect) 
% as well as color change (green - correct | red - incorrect | gray - absence of response) 
% of the (fake) fixation point (task doesn't require ocular fixation). 

% The subject initiates each trial by pressing the gamepad's primary button (the space bar in case of keyboard input)

% Returns a a topsTreeNode object which organizes tasks and trials.
% the object's run() method will start the task.  The object's gui() method
% will launch a graphical interface for viewing the organization of the
% task.

% Also returns as a second output a topsGroupedList object which holds all
% the objects and data needed to run the task, including tree.  The list's
% gui() method will launch a graphical interface for viewing the objects
% and data.

%Setting up the screen
sc = dotsTheScreen.theObject;
sc.reset('displayIndex', 0); %change display index to 0 for debug (small screen). 1 for full screen. Use >1 for external monitors.

%Call GetSecs just to load up the Mex files for getting time, so no delays later
GetSecs;

%% Setting up a list structure
list = topsGroupedList('AudiDeci_dynamic_HL');

% get subject id
subj_id = input('Subject ID: ','s');

%% time variables

iti = 1;
list{'timing'}{'intertrial'} = iti; %intertrial interval

%% trial variables
% generate conditions
c = topsConditions('AudiDeci_dynamic_HL');

parameter = 'freqType';
values = {'HNH','LNL'};
c.addParameter(parameter, values);

parameter = 'cohLevel';
values = {0.5 0.7 1};
c.addParameter(parameter, values);

parameter = 'changeTime';
values = {[0 300], [300 600], [600 900], [900 1200], [1200 1500], [1500 1800], [1800 2100]}; %  
c.addParameter(parameter, values);

likesFreqType = topsFoundation;
c.addAssignment('freqType', likesFreqType, '.', 'name');

likesCohLevel = topsFoundation;
c.addAssignment('cohLevel', likesCohLevel, '.', 'name');

likesChangeTime = topsFoundation;
c.addAssignment('changeTime', likesChangeTime, '.', 'name');

c.setPickingMethod('shuffled',nRep);

nCond = c.nConditions;
nTrials = nCond * nRep;

trialID = (1:nTrials)';
trialType = zeros(nTrials,1);
freqType = cell(nTrials,1);
cohLevel = zeros(nTrials,1);
changeTime = cell(nTrials,1);

keepGoing = true;
counter = 0;
while keepGoing
    counter = counter + 1;
    c.run();
    trialType(counter) = c.currentCondition;
    freqType{counter} = likesFreqType.name;
    cohLevel(counter) = likesCohLevel.name;
    changeTime{counter} = likesChangeTime.name;
%     disp(likesFreqType.name);
%     disp(likesCohLevel.name);
%     disp(likesChangeTime.name);
    keepGoing = ~c.isDone;
end


%% old version
% freqType_list = {'HNH','LNL'};%,'HSH','LSL','HLH','LHL'}; % 2|6
% cohLevel_list = [0.6 0.8 1]; % 3
% changeTime_list = [0 300; 300 600; 600 900; 900 1200; 1200 1500;1500 1800; 1800 2100]; % 7
% 
% nFreqType = length(freqType_list);
% nCohLevel = length(cohLevel_list);
% nChangeTime = size(changeTime_list,1);
% nCond = nFreqType * nCohLevel * nChangeTime;
% nTrials = nCond*nRep;

hd.loFreq = 1250; %hz      312.5 |  625 | 1250 | 2500 |  5000
hd.hiFreq = 5000; %hz     625   | 1250 | 2500 | 5000 | 10000
% hd.freqWidth = 1; %octave
% hd.nFreq = 7; %levels
hd.toneDur = 50; %ms 25 | 50
hd.toneSOA = 10; %ms  5 | 10
hd.trialDur = 2100; %ms
% freqType = 'HNH';
% cohLevel = 1;
% changeTime = [300 1000]; %ms
hd.changeRamp = 1; %ms

hd.fs = 100000;%384000;

% toneDurTot = hd.toneSOA+hd.toneDur;
% numTonePresentations = floor(hd.trialDur/toneDurTot);
% 
% % generate trial sequence
% trialType = table((1:nCond)',repmat(freqType_list',nCond/nFreqType,1),...
%     repmat(cohLevel_list',nCond/nCohLevel,1),...
%     repmat(changeTime_list,nCond/nChangeTime,1),'VariableNames',{'trialType','freqType','cohLevel','changeTime',});
% 
% trial_table = table((1:nTrials)',zeros(nTrials,1),'VariableNames',{'trialID','trialType'});
% ind = 0;
% for rr = 1:nRep
%     trial_table.trialType(ind+1:ind+nCond) = randperm(nCond)';
%     ind = ind+nCond;
% end
% 
% trial_table = outerjoin(trial_table,trialType,'MergeKey',1);
% trial_table = sortrows(trial_table,'trialID');
% 
% % get the tones ready
% multiplier = hd.fs/1000;
% stim = table(repmat({zeros(1,hd.trialDur*multiplier)},nTrials,1),repmat({zeros(1,numTonePresentations)},nTrials,1),repmat({zeros(1,numTonePresentations)},nTrials,1),'VariableNames',{'s','f','fID'});
% trial_table = [trial_table stim];
% waveforms = cell(1,nTrials);
% isH = zeros(1,nTrials);
% for tt = 1:height(trial_table)
%     freqType = trial_table.freqType{tt};
%     cohLevel = trial_table.cohLevel(tt);
%     changeTime = trial_table.changeTime(tt,:);
%     % generate tone in function?
%     [td,s,f,fID,h] = stimGen_dynamic_HL_toneClouds(hd.loFreq,hd.hiFreq,hd.toneDur,hd.toneSOA,hd.trialDur,freqType,hd.freqWidth,hd.nFreq,cohLevel,changeTime,hd.changeRamp,hd.fs);
%     waveforms{tt} = s;
%     isH(tt)= h;
%     trial_table.s{tt} = s;
%     trial_table.f{tt} = f;
%     trial_table.fID{tt} = fID;
% end
% 
% hd.nTones = numTonePresentations;
% hd.nRep = nRep;
% hd.nCond = nCond;
% % save trial_table
% disp(['Saving trial table to: ',mat_folder save_filename '.mat']);
% save([mat_folder save_filename '.mat'], 'trial_table','hd');

%%

%Creating audioplayer
player = dotsPlayableWave();
player.sampleFrequency = hd.fs;
player.duration = hd.trialDur/1000;
player.intensity = 0.5;

% add to the list:SUBJECT
list{'Subject'}{'ID'} = subj_id;

% COUNTER
list{'Counter'}{'trial'} = 0;

% STIMULUS
list{'Stimulus'}{'header'} = hd;
list{'Stimulus'}{'player'} = player;

list{'Stimulus'}{'trialType'} = trialType;
list{'Stimulus'}{'freqType'} = freqType;
list{'Stimulus'}{'cohLevel'} = cohLevel;
list{'Stimulus'}{'changeTime'} = changeTime;

list{'Stimulus'}{'waveforms'} = cell(1,nTrials);
list{'Stimulus'}{'isH'} = zeros(1,nTrials);

% TIMESTAMPS]
list{'Timestamps'}{'stim_start'} = zeros(1,nTrials);
list{'Timestamps'}{'stim_end'} = zeros(1,nTrials);
list{'Timestamps'}{'choices'} = zeros(1,nTrials);



%INPUT PARAMETERS
responsewindow = 2; %Time allowed to respond in, in seconds (must be < ITI)
% responsepattern = [4 2 4 2]; %was [4 2 4]
%(4 is right trigger or J, 2 is Left Trigger or F) --> So the subject has
%to pull the right trigger then the left then the right again to make it counts has a
%valid response. In that case only, Data.choices=1.

% INPUT
list{'Input'}{'responseWindow'} = responsewindow;
% list{'Input'}{'ResponsePattern'} = responsepattern;

list{'Input'}{'choices'} = zeros(1,nTrials);
list{'Input'}{'corrects'} = zeros(1,nTrials);


% Feedback 
pos_feedback = dotsPlayableFile();
pos_feedback.fileName = 'Coin.wav';
pos_feedback.intensity = 0.5;
neg_feedback = dotsPlayableFile();
neg_feedback.fileName = 'beep-02.wav';
neg_feedback.intensity = 0.5;

list{'Feedback'}{'pos'} = pos_feedback;
list{'Feedback'}{'neg'} = neg_feedback;

%%%%%%%%%%%%%%% not clear yet %%%%%%%%%%%%%%%
% % SYNCH
%     daq = labJack();
%     daqinfo.port = 0;
%     daqinfo.pulsewidth = 200; %milliseconds
%     list{'Synch'}{'DAQ'} = daq;
%     list{'Synch'}{'Info'} = daqinfo;
%     list{'Synch'}{'Times'} = [];
%     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% save the list
startsave(list);

%% Input
gp = dotsReadableHIDGamepad(); %Set up gamepad object

if gp.isAvailable
    
    % use the gamepad if connected
    ui = gp;
   
    % define movements, which must be held down
    %   map x-axis -1 to left and +1 to right
    isLeft = [gp.components.ID] == 7;
    isA = [gp.components.ID] == 3;
    isRight = [gp.components.ID] == 8;
    
    Left = gp.components(isLeft);
    A = gp.components(isA);
    Right = gp.components(isRight);
    
    gp.setComponentCalibration(Left.ID, [], [], [0 +2]);
    gp.setComponentCalibration(A.ID, [], [], [0 +3]);
    gp.setComponentCalibration(Right.ID, [], [], [0 +4]);
    
    % undefine any default events
    IDs = gp.getComponentIDs();
    for ii = 1:numel(IDs)
        gp.undefineEvent(IDs(ii));
    end
    
    %Define values for button presses
    gp.defineEvent(Left.ID, 'left', 0, 0, true);
    gp.defineEvent(A.ID, 'continue', 0, 0, true);
    gp.defineEvent(Right.ID, 'right', 0, 0, true);

else
    kb = dotsReadableHIDKeyboard(); %Use keyboard as last resort
    
    % define movements, which must be held down
    %   Left = +2, Up = +3, Right = +4
    isLeft = strcmp({kb.components.name}, 'KeyboardF');
    isSpace = strcmp({kb.components.name}, 'KeyboardSpacebar');
    isRight = strcmp({kb.components.name}, 'KeyboardJ');
    
    LeftKey = kb.components(isLeft);
    SpaceKey = kb.components(isSpace);
    RightKey = kb.components(isRight);
    
    kb.setComponentCalibration(LeftKey.ID, [], [], [0 +2]);
    kb.setComponentCalibration(SpaceKey.ID, [], [], [0 +3]);
    kb.setComponentCalibration(RightKey.ID, [], [], [0 +4]);
    
    % undefine any default events
    IDs = kb.getComponentIDs();
    for ii = 1:numel(IDs)
        kb.undefineEvent(IDs(ii));
    end
    
    % define events, which fire once even if held down
    % pressing w a d keys is a 'choice' event
    kb.defineEvent(LeftKey.ID, 'left',  0, 0, true);
    kb.defineEvent(SpaceKey.ID, 'continue',  0, 0, true);
    kb.defineEvent(RightKey.ID, 'right',  0, 0, true);
    
    ui = kb;
end

%Making sure the UI is running on the same clock as everything else!
%Using Operating System Time as absolute clock, from PsychToolbox
ui.clockFunction = @GetSecs;

%Storing ui in a List bin to access from functions!
ui.isAutoRead = 1;
list{'Input'}{'controller'} = ui;

%% Control:

% % the trunk of the tree, branches are added below
% tree = topsTreeNode('2afc task');
% tree.iterations = 1;
% tree.startFevalable = {@callObjectMethod, screen, @open};
% tree.finishFevalable = {@callObjectMethod, screen, @close};
% 
% % a batch of function calls that apply to all the trial types below
% %   start- and finishFevalable get called once per trial
% %   addCall() accepts fevalables to be called repeatedly during a trial
% trialCalls = topsCallList('call functions');
% trialCalls.addCall({@read, ui}, 'read input');
% list{'control'}{'trial calls'} = trialCalls;

%% Graphical ensemble

% Fixation point
    fix = dotsDrawableTargets();
    fix.colors = [1 1 1];
%     fix.pixelSize = 10;
    fix.isVisible = false;
    fix.width = 2;
    fix.height = 2;
    
    %Text prompts
    readyprompt = dotsDrawableText();
    readyprompt.string = 'Ready?';
    readyprompt.fontSize = 42;
    readyprompt.typefaceName = 'Calibri';
    readyprompt.isVisible = true;
    
    buttonprompt = dotsDrawableText();
    buttonprompt.string = 'press the space bar to get started';
    buttonprompt.fontSize = 24;
    buttonprompt.typefaceName = 'Calibri';
    buttonprompt.y = -2;
    buttonprompt.isVisible = true;  
    
    %Graphical ensemble
    ensemble = dotsEnsembleUtilities.makeEnsemble('Fixation Point', false);
    dot = ensemble.addObject(fix);
    ready = ensemble.addObject(readyprompt);
    button = ensemble.addObject(buttonprompt);


list{'Graphics'}{'ensemble'} = ensemble;
list{'Graphics'}{'dot Index'} = dot;

% tell the ensembles how to draw a frame of graphics
% the static drawFrame() takes a cell array of objects
ensemble.automateObjectMethod(...
    'draw', @dotsDrawable.drawFrame, {}, [], true);

% also put dotsTheScreen into its own ensemble
screen = dotsEnsembleUtilities.makeEnsemble('screen', false);
screen.addObject(dotsTheScreen.theObject());

% automate the task of flipping screen buffers
screen.automateObjectMethod('flip', @nextFrame);

%% Constant Calls
% Read User Interface constant call
readui = topsCallList();
readui.addCall({@read, ui}, 'Read the UI');

%% STATE MACHINE
white = @(index) ensemble.setObjectProperty('colors', [1 1 1], index);

show = @(index) ensemble.setObjectProperty('isVisible', true, index); %show asset
hide = @(index) ensemble.setObjectProperty('isVisible', false, index); %hide asset

% Prepare machine, for use in antetask
prepareMachine = topsStateMachine();
prepList = {'name', 'entry', 'input', 'exit', 'timeout', 'next';
    'Ready', {@startsave list},      {},      {@waitForCheckKey list},     0,       'Hide';
    'Hide', {hide [ready button]}, {}, {}, 0, 'Show';
    'Show', {show dot}, {}, {}, 0, 'Finish'
    'Finish', {}, {}, {}, 0, '';};
prepareMachine.addMultipleStates(prepList);


% State Machine, for use in maintask
Machine = topsStateMachine();
stateList = {'name', 'entry', 'input', 'exit', 'timeout', 'next';
                 'CheckReady', {white dot}, {}, {@waitForCheckKey list}, 0, 'Stimulus';
                 'Stimulus', {@playstim list}, {}, {@waitForChoiceKey list}, 0, 'Feedback';
                 'Feedback', {@showFeedback list}, {}, {}, 0, 'Exit';
                 'Exit', {@stopstim list}, {}, {}, iti, ''};
Machine.addMultipleStates(stateList);

conprep = topsConcurrentComposite();
conprep.addChild(ensemble);
conprep.addChild(prepareMachine);

contask = topsConcurrentComposite();
contask.addChild(ensemble);
contask.addChild(readui);
contask.addChild(Machine);

% Top Level Runnables
antetask = topsTreeNode();
antetask.addChild(conprep);

maintask = topsTreeNode();
maintask.iterations = nTrials;
maintask.addChild(contask);

task = topsTreeNode();
task.addChild(antetask);
task.addChild(maintask);
end

%% Accessory Functions

function showFeedback(list)
% hide the fixation point and cursor
ensemble = list{'Graphics'}{'ensemble'};
dot = list{'Graphics'}{'dot Index'};
counter = list{'Counter'}{'trial'};
choices = list{'Input'}{'choices'};

% compare stimulus direction to choice direction
isCorrect = list{'Input'}{'corrects'};

% indicate correct or incorrect by coloring in the targets
if isCorrect(counter)
    ensemble.setObjectProperty('colors', [.25 0.75 0.1], dot);
    feedback = list{'Feedback'}{'pos'};
else
    ensemble.setObjectProperty('colors', [0.75 0.25 0.1], dot);
    feedback = list{'Feedback'}{'neg'};
end
feedback.prepareToPlay;
feedback.play;
end

function string = waitForChoiceKey(list)
    % Getting list items
    choices = list{'Input'}{'choices'};
    counter = list{'Counter'}{'trial'};
    ui = list{'Input'}{'controller'};
    player = list{'Stimulus'}{'player'};
    isH = list{'Stimulus'}{'isH'}; % whether it's a high-freq trial
    responsewindow = list{'Input'}{'responseWindow'};
    playsecs = (length(player.wave)/player.sampleFrequency)*6;
    cur_f = isH(counter) + 1; % isH : 2 - high | 1 - low
    ui.flushData
    
  %Initializing variable
    press = '';
    
    %Waiting for keypress
    tic 
    while ~strcmp(press, 'left') && ~strcmp(press, 'right')
        %Break loop if responsewindow expires and move to next trial
        if toc > responsewindow %This was previously Playsecs
            choice = NaN;
            timestamp = NaN;
            break
        end
        
        %Check for button press
        press = '';
        read(ui);
        [~, ~, eventname, ~] = ui.getHappeningEvent();
        if ~isempty(eventname) && length(eventname) == 1
            press = eventname;
            player.stop;
        end
    end
    
    %Updating choices list
    if strcmp(press, 'right')
        choice = 1;
    elseif strcmp(press, 'left')
        choice = 2;
    end
    
    choices(counter+1) = choice;
    list{'Input'}{'choices'} = choices;
        
    %Updating timestamps list
    if ~isempty(press)
        timestamp = ui.history;
        timestamp = timestamp(timestamp(:, 2) > 1, :); %Just to make sure I get a timestamp from a pressed key/button
        timestamp = timestamp(end);
        stim_start = list{'Timestamps'}{'stim_start'};
        rt = (timestamp - stim_start(counter))*1000; %ms
        cur_choice = press{1};
    else
        rt = nan;
        cur_choice = nan;
    end
    
    timestamps = list{'Timestamps'}{'choices'};
    timestamps(counter) = timestamp;
    list{'Timestamps'}{'choices'} = timestamps;
    
    
    if cur_f == choice
        correct = 1;
        string = 'Correct';
    else
        correct = 0;
        string = 'Incorrect';
    end
    
    corrects = list{'Input'}{'corrects'};
    corrects(counter) = correct;
    list{'Input'}{'corrects'} = corrects;
    
    fprintf('Trial %d complete. Choice: %s (%s). RT: %s \n', counter, cur_choice, string, rt);
end


function waitForCheckKey(list)
    % Getting list items
    ui = list{'Input'}{'controller'};
    ui.flushData;
    
    %Initializing variable
    press = '';
  
    %Waiting for keypress
    while ~strcmp(press, 'continue')
        press = '';
        read(ui);
        [~, ~, eventname, ~] = ui.getHappeningEvent();
        if ~isempty(eventname) && length(eventname) == 1
            press = eventname;
        end
    end
end

function playstim(list)
    %Adding current iteration to counter
    counter = list{'Counter'}{'trial'};
    counter = counter + 1;
    list{'Counter'}{'trial'} = counter; 
    
    freq_list = list{'Stimulus'}{'freqType'};
    coh_list = list{'Stimulus'}{'cohLevel'};
    time_list = list{'Stimulus'}{'changeTime'};
    
    freqType = freq_list{counter};
    cohLevel = coh_list(counter);
    changeTime = time_list{counter};
    
    hd = list{'Stimulus'}{'header'};
    
    [td,waveform,f,h] = stimGen_dynamic_HL_v2(hd.loFreq,hd.hiFreq,hd.toneDur,hd.toneSOA,hd.trialDur,freqType,cohLevel,changeTime,hd.changeRamp,hd.fs);

    
    %importing important list objects
    player = list{'Stimulus'}{'player'};    
    player.wave = waveform;
    player.prepareToPlay;
    
    %Logging timestamps of the stimulus
    stim_start = list{'Timestamps'}{'stim_start'};
    stim_start(counter) = player.play;
    list{'Timestamps'}{'stim_start'} = stim_start;
    
    waveforms = list{'Stimulus'}{'waveforms'};
    waveforms{counter} = waveform;
    list{'Stimulus'}{'waveforms'} = waveforms;
    
    isH = list{'Stimulus'}{'isH'};
    isH(counter) = h;
    list{'Stimulus'}{'isH'} = isH;
    
    fprintf('Trial %d', counter)
end

function stopstim(list)
    %Get sound player
    counter = list{'Counter'}{'trial'};
    player = list{'Stimulus'}{'player'};
   
    %Get timestamp
    stim_stop = list{'Timestamps'}{'stim_stop'};
    stim_stop(counter) = player.stopTime;
    list{'Timestamps'}{'stim_stop'} = stim_stop;
end


function pulser(list)
    %import list objects
    daq = list{'Synch'}{'DAQ'};
    daqinfo = list{'Synch'}{'Info'};
    
    %send pulse
    time = GetSecs; %getting timestamp. Cmd-response for labjack is <1ms
    daq.timedTTL(daqinfo.port, daqinfo.pulsewidth);
    disp('Pulse Sent');
    
    %logging timestamp
    list{'Synch'}{'Times'} = [list{'Synch'}{'Times'}; time];
end


function startsave(list)
    %creates a viable savename for use outside of function, to save file
    ID = list{'Subject'}{'ID'};
    appendno = 0;
    savename = [ID num2str(appendno) '_audiDeci_dynamic_HL'];
    
    %Checking if file already exists, if so, changes savename by appending
    %a number
    while exist([savename '.mat'])
        appendno = appendno + 1;
        savename = [ID num2str(appendno) '_audiDeci_dynamic_HL'];
    end
    
    list{'Subject'}{'Savename'} = savename;
end