function [task, list] = AudiDeci_prior_HL(disp_ind)

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
sc.reset('displayIndex', disp_ind); %change display index to 0 for debug (small screen). 1 for full screen. Use >1 for external monitors.

%Call GetSecs just to load up the Mex files for getting time, so no delays later
GetSecs;

%% Setting up a list structure
list = topsGroupedList('AudiDeci_prior_HL');

% get subject id
subj_id = input('Subject ID: ','s');
cur_date = datestr(now,'yymmdd');
cur_time = datestr(now,'HHMM');
cur_task = mfilename;
save_filename = [cur_task '_' subj_id '_' cur_date '_' cur_time];

% SUBJECT
list{'meta'}{'subjID'} = subj_id;
list{'meta'}{'date'} = cur_date;
list{'meta'}{'time'} = cur_time;
list{'meta'}{'task'} = cur_task;
list{'meta'}{'save_filename'} = save_filename;

%% trial variables
% generate conditions
nRep = 48;

taskConditions = topsConditions(cur_task);

prior_parameter = 'priorLevel';
prior_values = {-3 -2 -1 0 1 2 3};
taskConditions.addParameter(prior_parameter, prior_values);

likesPriorLevel = topsFoundation();
taskConditions.addAssignment('priorLevel', likesPriorLevel, '.', 'name');

taskConditions.setPickingMethod('shuffled',nRep);

nCond = taskConditions.nConditions;
nTrials = nCond * nRep;

priorLevels = zeros(nTrials,1);

keepGoing = true;
counter = 0;
while keepGoing
    counter = counter + 1;
    taskConditions.run();
    priorLevels(counter) = likesPriorLevel.name;
    keepGoing = ~taskConditions.isDone;
end

list{'control'}{'task conditions'} = taskConditions;
list{'control'}{'priorLevels'} = priorLevels;

% generate cohLevels for each priorLevel
coh_list = [0.2 0.35 0.5 0.65 0.8];
cohLevels = zeros(size(priorLevels));
for cc = 1:nCond
    ind = find(priorLevels == prior_values{cc});
    tmp_coh = topsConditions('coh');
    switch prior_values{cc}
        case -3, coh_values = [repmat(coh_list(1),1,6) repmat(coh_list(2),1,6) repmat(coh_list(3),1,4)]; % 12 low + 4 noise
        case -2, coh_values = [repmat(coh_list(1),1,5) repmat(coh_list(2),1,5) repmat(coh_list(3),1,4) coh_list(4) coh_list(5)]; % 10 low + 4 noise + 2 high
        case -1, coh_values = [repmat(coh_list(1),1,4) repmat(coh_list(2),1,4) repmat(coh_list(3),1,4) repmat(coh_list(4),1,2) repmat(coh_list(5),1,2)]; % 8 low + 4 noise + 4 high
        case  0, coh_values = [repmat(coh_list(1),1,3) repmat(coh_list(2),1,3) repmat(coh_list(3),1,4) repmat(coh_list(4),1,3) repmat(coh_list(5),1,3)]; % 6 low + 4 noise + 6 high
        case  1, coh_values = [repmat(coh_list(1),1,2) repmat(coh_list(2),1,2) repmat(coh_list(3),1,4) repmat(coh_list(4),1,4) repmat(coh_list(5),1,4)]; % symmetric to above
        case  2, coh_values = [coh_list(1) coh_list(2) repmat(coh_list(3),1,4) repmat(coh_list(4),1,5) repmat(coh_list(5),1,5)];
        case  3, coh_values = [repmat(coh_list(3),1,4) repmat(coh_list(4),1,6) repmat(coh_list(5),1,6)];
    end
    coh_values = num2cell(coh_values);
    tmp_coh.addParameter('cohLevel', coh_values);
    likesCohLevel = topsFoundation();
    tmp_coh.addAssignment('cohLevel',likesCohLevel, '.', 'name');
    tmp_coh.setPickingMethod('shuffled',nRep/length(coh_values));
    
    keepGoing = true;
    counter = 0;
    while keepGoing
        counter = counter + 1;
        tmp_coh.run();
        cohLevels(ind(counter)) = likesCohLevel.name;
        keepGoing = ~tmp_coh.isDone;
    end
end

list{'control'}{'cohLevels'} = cohLevels;
%% audio settings

hd.loFreq = 880; %hz      312.5 |  625 | 1250 | 2500 |  5000
hd.hiFreq = 3520; %hz     625   | 1250 | 2500 | 5000 | 10000 
hd.toneDur = 50; %ms 25 | 50
hd.toneSOA = 10; %ms  5 | 10
hd.trialDur = 2100; %ms

hd.fs = 100000;%384000;

%INPUT PARAMETERS
responsewindow = hd.trialDur/1000; %Time allowed to respond in, in seconds (== stim duration)
list{'Input'}{'responseWindow'} = responsewindow;

%Creating audioplayer
player = dotsPlayableWave();
player.sampleFrequency = hd.fs;
player.duration = hd.trialDur/1000; %sec
player.intensity = 0.5;

% Feedback 
pos_feedback = dotsPlayableFile();
pos_feedback.fileName = 'Coin.wav';
pos_feedback.intensity = 0.5;
neg_feedback = dotsPlayableFile();
neg_feedback.fileName = 'beep-02.wav';
neg_feedback.intensity = 0.5;

list{'Feedback'}{'pos'} = pos_feedback;
list{'Feedback'}{'neg'} = neg_feedback;

%% time variables

iti = 1;
list{'timing'}{'intertrial'} = iti; %intertrial interval
list{'timing'}{'preCue'} = 1;
list{'timing'}{'feedback'} = 1;

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

%% add to the list

% COUNTER
list{'Counter'}{'trial'} = 0;

% STIMULUS
list{'Stimulus'}{'header'} = hd;
list{'Stimulus'}{'player'} = player;

list{'Stimulus'}{'waveforms'} = cell(1,nTrials);
list{'Stimulus'}{'isH'} = zeros(1,nTrials);

% TIMESTAMPS]
list{'Timestamps'}{'stim_start'} = zeros(1,nTrials);
list{'Timestamps'}{'stim_stop'} = zeros(1,nTrials);
list{'Timestamps'}{'choices'} = zeros(1,nTrials);

% INPUT
list{'Input'}{'choices'} = zeros(1,nTrials);
list{'Input'}{'corrects'} = zeros(1,nTrials);
list{'Input'}{'RT'} = zeros(1,nTrials);


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


%% Graphics

list{'Graphics'}{'preCue width'} = 1;
list{'Graphics'}{'preCue height'} = 5;
list{'Graphics'}{'white'} = [1 1 1];
list{'Graphics'}{'gray'} = [0.5 0.5 0.5];
list{'Graphics'}{'red'} = [0.75 0.25 0.1];
list{'Graphics'}{'green'} = [.25 0.75 0.1];

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

% pre_cue object - a long vertical box with a horizontal line - line level indicates prior
preCue_box = dotsDrawableTargets();
% a regtangle
preCue_box.xCenter = 0;
preCue_box.yCenter = 0;
preCue_box.nSides = 4;
% fill in variables
preCue_box.colors = list{'Graphics'}{'gray'};
preCue_box.height = list{'Graphics'}{'preCue height'};
preCue_box.width = list{'Graphics'}{'preCue width'};
preCue_box.isVisible = false;
list{'Graphics'}{'preCue_box'} = preCue_box;

preCue_line = dotsDrawableLines();
% a horizontal line - length > box width
preCue_line.pixelSize = 3;
preCue_line.xFrom = -preCue_box.width;
preCue_line.xTo = preCue_box.width;
preCue_line.colors = list{'Graphics'}{'white'};
% line level from prior - default value = 0
preCue_line.yFrom = 0;
preCue_line.yTo = 0;
preCue_line.isVisible = false;
list{'Graphics'}{'preCue_line'} = preCue_line;

% a cursor dot to indicate user selection
cursor = dotsDrawableTargets();
cursor.colors = list{'Graphics'}{'gray'};
cursor.width = 1.5;
cursor.height = 1.5;
cursor.xCenter = 0;
cursor.yCenter = 0;
cursor.isVisible = false;
list{'Graphics'}{'cursor'} = cursor;

%Graphical ensemble
ensemble = dotsEnsembleUtilities.makeEnsemble('Fixation Point', false);
box = ensemble.addObject(preCue_box);
line = ensemble.addObject(preCue_line);
target = ensemble.addObject(cursor);
ready = ensemble.addObject(readyprompt);
button = ensemble.addObject(buttonprompt);

list{'Graphics'}{'ensemble'} = ensemble;
list{'Graphics'}{'box'} = box;
list{'Graphics'}{'line'} = line;
list{'Graphics'}{'target'} = target;

% tell the ensembles how to draw a frame of graphics
% the static drawFrame() takes a cell array of objects
ensemble.automateObjectMethod(...
    'draw', @dotsDrawable.drawFrame, {}, [], true);

% also put dotsTheScreen into its own ensemble
screen = dotsEnsembleUtilities.makeEnsemble('screen', false);
screen.addObject(dotsTheScreen.theObject());
list{'Graphics'}{'screen'} = screen;

% automate the task of flipping screen buffers
screen.automateObjectMethod('flip', @nextFrame);

%% Control:

% a batch of function calls that apply to all the trial types below
%   start- and finishFevalable get called once per trial
%   addCall() accepts fevalables to be called repeatedly during a trial
trialCalls = topsCallList();
trialCalls.addCall({@read, ui}, 'read input');
list{'control'}{'trial calls'} = trialCalls;

%% STATE MACHINE
show = @(index) ensemble.setObjectProperty('isVisible', true, index); %show asset
hide = @(index) ensemble.setObjectProperty('isVisible', false, index); %hide asset

% Prepare machine, for use in antetask
prepareMachine = topsStateMachine();
prepStates = {'name', 'entry', 'input', 'exit', 'timeout', 'next';
    'Ready', {},      {},      {@waitForCheckKey list},     0,       'Hide';
    'Hide', {hide [ready button]}, {}, {}, 0, 'Show';
    'Show', {show box}, {}, {}, 0, 'Finish'
    'Finish', {}, {}, {}, 0, '';};
prepareMachine.addMultipleStates(prepStates);

list{'control'}{'prepareMachine'} = prepareMachine;

% State Machine, for use in maintask
mainMachine = topsStateMachine();
mainStates = {'name', 'entry', 'input', 'exit', 'timeout', 'next';
                 'CheckReady', {@startTrial list}, {}, {@waitForCheckKey list}, 0, 'Stimulus';
                 'Stimulus', {@playstim list}, {}, {@waitForChoiceKey list}, 0, 'Feedback';
                 'Feedback', {@showFeedback list}, {}, {}, 0, 'Exit';
                 'Exit',{@finishTrial list}, {}, {}, iti,''};
mainMachine.addMultipleStates(mainStates);

list{'control'}{'mainMachine'} = mainMachine;

prepareConcurrents = topsConcurrentComposite();
prepareConcurrents.addChild(ensemble);
prepareConcurrents.addChild(prepareMachine);
prepareConcurrents.addChild(screen);

% add a branch to the tree trunk to lauch a Fixed Time trial
prepareTree = topsTreeNode();
prepareTree.addChild(prepareConcurrents);

mainConcurrents = topsConcurrentComposite();
mainConcurrents.addChild(ensemble);
mainConcurrents.addChild(trialCalls);
mainConcurrents.addChild(mainMachine);
mainConcurrents.addChild(screen);

mainTree = topsTreeNode();
mainTree.iterations = nTrials;
mainTree.addChild(mainConcurrents);

% Top Level Runnables
task = topsTreeNode();
task.startFevalable = {@callObjectMethod, screen, @open};
task.finishFevalable = {@callObjectMethod, screen, @close};
task.addChild(prepareTree);
task.addChild(mainTree);
end

%% Accessory Functions

function startTrial(list)
% clear data from the last trial
ui = list{'Input'}{'controller'};
ui.flushData();

counter = list{'Counter'}{'trial'};
counter = counter + 1;
list{'Counter'}{'trial'} = counter;

priorLevels = list{'control'}{'priorLevels'};
ensemble = list{'Graphics'}{'ensemble'};
line = list{'Graphics'}{'line'};
box = list{'Graphics'}{'box'};
% set level of vertical line to prior level
yLevel = priorLevels(counter)/2;
ensemble.setObjectProperty('yFrom', yLevel,line);
ensemble.setObjectProperty('yTo', yLevel, line);
ensemble.setObjectProperty('isVisible', true, line);
ensemble.setObjectProperty('isVisible', true, box);
end

function finishTrial(list)
ensemble = list{'Graphics'}{'ensemble'};
target = list{'Graphics'}{'target'};
box = list{'Graphics'}{'box'};
ensemble.setObjectProperty('isVisible', false, target);
ensemble.setObjectProperty('isVisible', false, box);
% only need to wait our the intertrial interval
pause(list{'timing'}{'intertrial'});
end

function showFeedback(list)
% hide the fixation point and cursor
ensemble = list{'Graphics'}{'ensemble'};
target = list{'Graphics'}{'target'};
counter = list{'Counter'}{'trial'};

% compare stimulus direction to choice direction
isCorrect = list{'Input'}{'corrects'};

% indicate correct or incorrect by coloring in the targets
if isCorrect(counter)
    ensemble.setObjectProperty('colors', list{'Graphics'}{'green'}, target);
    feedback = list{'Feedback'}{'pos'};
else
    ensemble.setObjectProperty('colors', list{'Graphics'}{'red'}, target);
    feedback = list{'Feedback'}{'neg'};
end
feedback.prepareToPlay;
feedback.play;
end

function string = waitForChoiceKey(list)
    % Getting list items
    ensemble = list{'Graphics'}{'ensemble'};
    target = list{'Graphics'}{'target'};
    choices = list{'Input'}{'choices'};
    counter = list{'Counter'}{'trial'};
    ui = list{'Input'}{'controller'};
    player = list{'Stimulus'}{'player'};
    isH = list{'Stimulus'}{'isH'}; % whether it's a high-freq trial
    responsewindow = list{'Input'}{'responseWindow'};
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
    
    %Get timestamp
    stim_stop = list{'Timestamps'}{'stim_stop'};
    stim_stop(counter) = player.stopTime;
    list{'Timestamps'}{'stim_stop'} = stim_stop;
    
    %Updating choices list
    if strcmp(press, 'right')
        choice = 1;
        ensemble.setObjectProperty( ...
            'xCenter', 5, target);
        ensemble.setObjectProperty('isVisible', true, target);
    elseif strcmp(press, 'left')
        choice = 2;
        ensemble.setObjectProperty( ...
            'xCenter', -5, target);
        ensemble.setObjectProperty('isVisible', true, target);
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
    
    RTs = list{'Input'}{'RT'};
    RTs(counter) = rt;
    list{'Input'}{'RT'} = RTs;
    
    fprintf('Trial %d complete. Choice: %s (%s). RT: %3.3f \n', counter, cur_choice, string, rt);
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
    ensemble = list{'Graphics'}{'ensemble'};
    line = list{'Graphics'}{'line'};
    ensemble.setObjectProperty('isVisible', false, line);
    
    counter = list{'Counter'}{'trial'};
    coh_list = list{'control'}{'cohLevels'};
    cohLevel = coh_list(counter);
    
    hd = list{'Stimulus'}{'header'};
    
    [td,waveform,h] = stimGen_static_HL(hd.loFreq,hd.hiFreq,hd.toneDur,hd.toneSOA,hd.trialDur,cohLevel,hd.fs);

    %importing important list objects
    player = list{'Stimulus'}{'player'};    
    player.wave = waveform;
    player.prepareToPlay;
    
    player.play
    
    %Logging timestamps of the stimulus
    stim_start = list{'Timestamps'}{'stim_start'};
    stim_start(counter) = player.playTime;
    list{'Timestamps'}{'stim_start'} = stim_start;
    
    waveforms = list{'Stimulus'}{'waveforms'};
    waveforms{counter} = waveform;
    list{'Stimulus'}{'waveforms'} = waveforms;
    
    isH = list{'Stimulus'}{'isH'};
    isH(counter) = h;
    list{'Stimulus'}{'isH'} = isH;
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
