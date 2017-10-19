function [task, list] = AudiDeci_noise_embedded_HL_ampCalib(dispInd,subjID,eyeTrackerOn)


% 20170428: update for prior/pretone task + eye link
% 20170322: created by Lalitta - auditory decision task - 
% 2-alternative forced choice task: low-high frequency discrimination
% stimuli consist of series of tones embeded in noise the last of which is
% a test tone with an amplitude that varies across trials -> to build a
% psychometric curve for each subject & find critical amplitude for H/L
% stimulus-response combinations: high - left | low - right 
% using gamepade (L/R buttons) or keyboard (F/J keys; if a HID gamepad is not connected)

% Feedback is given by auditory signal (coin sound - correct, low-pitched beep - incorrect) 
% as well as color (green - correct | red - incorrect | gray - absence of response) 
% of the target (task doesn't require ocular fixation). 

% The subject initiates each trial by pressing the gamepad's primary button (the space bar in case of keyboard input)

% Returns a a topsTreeNode object which organizes tasks and trials.
% the object's run() method will start the task.  The object's gui() method
% will launch a graphical interface for viewing the organization of the
% task.

% Also returns as a second output a topsGroupedList object which holds all
% the objects and data needed to run the task, including tree.  The list's
% gui() method will launch a graphical interface for viewing the objects
% and data.

%%
% if nargin < 1
%     disp_ind = 0;
%     isClient = false;
% elseif nargin < 2
    isClient = false;
% end
if nargin < 2
    subjID = input('Subject ID:','s');
end
if nargin < 3
    eyeTrackerOn = 0;
end



%% Setting up the screen
sc = dotsTheScreen.theObject;
sc.reset('displayIndex', dispInd); %change display index to 0 for debug (small screen). 1 for full screen. Use >1 for external monitors.

%Call GetSecs just to load up the Mex files for getting time, so no delays later
GetSecs;

% get subject id
disp(['Run audio calibration for: ' subjID]);
curDate = datestr(now,'yymmdd');
curTime = datestr(now,'HHMM');
curTask = mfilename;
saveFilename = [subjID '_' curDate '_' curTime '_' curTask];

%% Setting up a list structure
list = topsGroupedList(curTask);

% SUBJECT
list{'meta'}{'subjID'} = subjID;
list{'meta'}{'date'} = curDate;
list{'meta'}{'time'} = curTime;
list{'meta'}{'task'} = curTask;
list{'meta'}{'saveFilename'} = saveFilename;

questVersion = 1;
list{'meta'}{'questVersion'} = questVersion;
%% settings for generating sequence of conditions
nTrials = 50;
pick_method = 'coin-toss';
sequenceTypes = {'H','L'};

taskConditions = topsConditions(curTask);
sequenceParameter = 'sequenceType';
taskConditions.addParameter(sequenceParameter, sequenceTypes);

likesSequence = topsFoundation();
taskConditions.addAssignment('sequenceType', likesSequence, '.', 'name');

switch pick_method
    case 'shuffledEach'
        taskConditions.setPickingMethod('shuffledEach',nTrials);
        taskConditions.run();
        taskConds = cell(nTrials,1);
        for ii = 1:nTrials
            taskConds{ii} = sequenceTypes{taskConditions.pickSequence(ii)};
        end
    case 'coin-toss'
        taskConditions.setPickingMethod('coin-toss');
        taskConditions.maxPicks = nTrials;
        taskConds = cell(nTrials,1);
        keepGoing = true;
        counter = 1;
        while keepGoing
            taskConditions.run();
            taskConds{counter} = likesSequence.name;
            keepGoing = ~taskConditions.isDone;
            counter = counter + 1;
        end
end

trialVarSequence = taskConds;

list{'Control'}{'trialVarSequence'} = trialVarSequence;

%% audio settings

hd.loFreq = 250; %hz      312.5 |  625 | 1250 | 2500 |  5000
hd.hiFreq = 2000; %hz     625   | 1250 | 2500 | 5000 | 10000 
hd.toneDur = 300; %ms 25 | 50
hd.toneIBI = 100; %ms  5 | 10
% hd.trialDur = 2100; %ms

hd.fs = 44100;%384000;

%INPUT PARAMETERS
responsewindow = hd.toneDur/1000 + 3; %Time allowed to respond in, in seconds (== stim duration)
list{'Input'}{'responseWindow'} = responsewindow;

%Creating audioplayer
player = dotsPlayableWave();
player.sampleFrequency = hd.fs;
% player.duration = hd.trialDur/1000; %sec
player.intensity = 0.1;

% Feedback 
pos_feedback = dotsPlayableFile();
pos_feedback.fileName = 'Coin.wav';
pos_feedback.intensity = 0.2;
neg_feedback = dotsPlayableFile();
neg_feedback.fileName = 'beep-02.wav';
neg_feedback.intensity = 0.2;

list{'Feedback'}{'pos'} = pos_feedback;
list{'Feedback'}{'neg'} = neg_feedback;

%% QUEST OBJECT
% Creating Quest structure

if questVersion == 1
    tGuess = 0.1; %Guess at the appropriate amplitude
    tGuessSd = 0.5; %Standard deviation in guesses permitted
    
    pThreshold = 0.6; %How successful do you want a subject to be?
    
    beta = 3.5;
    delta = 0.1;
    gamma = 0.5;
    
    q = QuestCreate(tGuess, tGuessSd, pThreshold, beta, delta, gamma, 0.005, 0.5); %Quest object created
    
    q.normalizePdf=1;
% initialize quest for high & low amplitude separately  
    list{'Quest'}{'ObjectH'} = q;
    list{'Quest'}{'ObjectL'} = q;
elseif questVersion == 2
    tGuess = 0; %Guess at the appropriate amplitude
    tGuessSd = 1; %Standard deviation in guesses permitted
    
    pThreshold = 0.6; %How successful do you want a subject to be?
    
    beta = 3.5;
    delta = 0.1;
    gamma = 0.5;
    
    q = QuestCreate(tGuess, tGuessSd, pThreshold, beta, delta, gamma, 0.01, 0.5); %Quest object created
    
    q.normalizePdf=1;
    list{'Quest'}{'Object'} = q;
end

%% time variables

iti = 1;
list{'timing'}{'intertrial'} = iti; %intertrial interval

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

list{'Input'}{'Controller'} = ui;

%% EYELINK  
list{'Eyelink'}{'eyeTrackerOn'} = eyeTrackerOn;

if eyeTrackerOn
list{'Eyelink'}{'Fixtime'} = 0.1;
list{'Eyelink'}{'SamplingFreq'} = 1000;

screensize = get(0, 'MonitorPositions');
screensize = screensize(1, [3, 4]);
centers = screensize/2;
list{'Eyelink'}{'Centers'} = centers;
list{'Eyelink'}{'Invalid'} = -32768;

%Setting windows for fixation:
window_width = 0.5 * screensize(1);%0.3*screensize(1);
window_height = 0.5 * screensize(2);%0.3*screensize(2);

xbounds = [centers(1) - window_width/2, centers(1) + window_width/2];
ybounds = [centers(2) - window_height/2, centers(2) + window_height/2];

list{'Eyelink'}{'XBounds'} = xbounds;
list{'Eyelink'}{'YBounds'} = ybounds;

list{'Eyelink'}{'trialStartTimestamps'} = zeros(nTrials,1);
list{'Eyelink'}{'stimOnTimestamps'} = zeros(nTrials,1);
list{'Eyelink'}{'preStimOnTimestamps'} = zeros(nTrials,1);
list{'Eyelink'}{'postStimOnTimestamps'} = zeros(nTrials,1);
list{'Eyelink'}{'responseTimestamps'} = zeros(nTrials,1);
list{'Eyelink'}{'postResponseTimestamps'} = zeros(nTrials,1);
list{'Eyelink'}{'trialStopTimestamps'} = zeros(nTrials,1);

end
%% add to the list

% COUNTER
list{'Counter'}{'nTrials'} = nTrials;
list{'Counter'}{'trial'} = 0;

% STIMULUS
list{'Stimulus'}{'header'} = hd;
list{'Stimulus'}{'player'} = player;

list{'Stimulus'}{'waveforms'} = cell(nTrials,1);
list{'Stimulus'}{'freq'} = cell(nTrials,1);
list{'Stimulus'}{'isH'} = zeros(nTrials,1);
list{'Stimulus'}{'testAmps'} = zeros(nTrials,1);

% TIMESTAMPS]
list{'Timestamps'}{'trialStarts'} = zeros(nTrials,1);
list{'Timestamps'}{'trialStops'} = zeros(nTrials,1);
list{'Timestamps'}{'stimStarts'} = zeros(nTrials,1);
list{'Timestamps'}{'stimStops'} = zeros(nTrials,1);
list{'Timestamps'}{'choices'} = zeros(nTrials,1);

% INPUT
list{'Input'}{'choices'} = zeros(nTrials,1);
list{'Input'}{'corrects'} = zeros(nTrials,1);
list{'Input'}{'RT'} = zeros(nTrials,1);

%% Graphics

list{'Graphics'}{'preCue width'} = 0.8;
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
buttonprompt.string = 'press A to get started';
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
ensemble = dotsEnsembleUtilities.makeEnsemble('drawables', isClient);
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
screen = dotsEnsembleUtilities.makeEnsemble('screen', isClient);
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
list{'Control'}{'trial calls'} = trialCalls;

%% STATE MACHINE
show = @(index) ensemble.setObjectProperty('isVisible', true, index); %show asset
hide = @(index) ensemble.setObjectProperty('isVisible', false, index); %hide asset

% Prepare machine, for use in antetask
prepareMachine = topsStateMachine();
prepStates = {'name', 'entry', 'input', 'exit', 'timeout', 'next';
    'Ready', {},      {},      {@waitForCheckKey list},     0,       'Hide';
    'Hide', {hide [ready button]}, {}, {}, 0, 'Show';
    'Show', {show [box line]}, {}, {}, 0, 'Finish'
    'Finish', {}, {}, {}, 0, '';};
prepareMachine.addMultipleStates(prepStates);

list{'Control'}{'prepareMachine'} = prepareMachine;

% State Machine, for use in calibTask
calibMachine = topsStateMachine();
calibStates = {'name', 'entry', 'input', 'exit', 'timeout', 'next';
                 'CheckReady', {@startTrial list}, {}, {@waitForCheckKey list}, 0, 'Stimulus';
                 'Stimulus', {@playstim list}, {}, {@waitForChoiceKey list}, 0, 'Feedback';
                 'Feedback', {@showFeedback list}, {}, {}, 0, 'Exit';
                 'Exit',{@finishTrial list}, {}, {}, 0,''};
calibMachine.addMultipleStates(calibStates);

list{'Control'}{'calibMachine'} = calibMachine;


prepareConcurrents = topsConcurrentComposite();
prepareConcurrents.addChild(ensemble);
prepareConcurrents.addChild(prepareMachine);
prepareConcurrents.addChild(screen);

prepareTree = topsTreeNode();
prepareTree.addChild(prepareConcurrents);

calibConcurrents = topsConcurrentComposite();
calibConcurrents.addChild(ensemble);
calibConcurrents.addChild(calibMachine);
calibConcurrents.addChild(screen);

calibTree = topsTreeNode();
calibTree.addChild(calibConcurrents);
calibTree.iterations = nTrials;
list{'Control'}{'calibTree'} = calibTree;

% Top Level Runnables
task = topsTreeNode();
task.startFevalable = {@callObjectMethod, screen, @open};
task.finishFevalable = {@callObjectMethod, screen, @close};
task.addChild(prepareTree);
task.addChild(calibTree);
end

%% Accessory Functions

function startTrial(list)
% clear data from the last trial
ui = list{'Input'}{'Controller'};
ui.flushData();

counter = list{'Counter'}{'trial'};
counter = counter + 1;
list{'Counter'}{'trial'} = counter;

trialStarts = list{'Timestamps'}{'trialStarts'};
trialStarts(counter) = mglGetSecs;
list{'Timestamps'}{'trialStarts'} = trialStarts;
eyeTrackerOn = list{'Eyelink'}{'eyeTrackerOn'};
if eyeTrackerOn
    Eyelink('Message', ['trialStart_' num2str(counter) '_' num2str(mglGetSecs)]);
    trialStartTimestamps = list{'Eyelink'}{'trialStartTimestamps'};
    newsample = Eyelink('NewestFloatSample');
    trialStartTimestamps(counter) = newsample.time;
    list{'Eyelink'}{'trialStartTimestamps'} = trialStartTimestamps;
end

ensemble = list{'Graphics'}{'ensemble'};
line = list{'Graphics'}{'line'};
box = list{'Graphics'}{'box'};

ensemble.setObjectProperty('isVisible', true, box);
ensemble.setObjectProperty('isVisible', false, line);
ensemble.setObjectProperty('colors', list{'Graphics'}{'white'}, line);
end

function waitForCheckKey(list)
% Getting list items
ui = list{'Input'}{'Controller'};
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
if list{'Eyelink'}{'eyeTrackerOn'}
    list{'Eyelink'}{'FixVal'} = 0;
    checkFixation(list)
end

ensemble = list{'Graphics'}{'ensemble'};
line = list{'Graphics'}{'line'};
ensemble.setObjectProperty('isVisible', true, line);
end

function checkFixation(list)
fixtime = list{'Eyelink'}{'Fixtime'};
fs = list{'Eyelink'}{'SamplingFreq'};
invalid = list{'Eyelink'}{'Invalid'};
xbounds = list{'Eyelink'}{'XBounds'};
ybounds = list{'Eyelink'}{'YBounds'};

counter = list{'Counter'}{'trial'};

FixVal = list{'Eyelink'}{'FixVal'};

fixms = fixtime*fs; %Getting number of fixated milliseconds needed

%Initializing the structure that temporarily holds eyelink sample data
%Ensuring eyestruct does not get prohibitively large.
%After 30 seconds it will shift the last second to the first and continue.
eyeStructSize = 30000;
eyestruct = Eyelink('NewestFloatSample');
eyestruct = repmat(eyestruct,eyeStructSize,1);
fcounter = 1;

while FixVal == 0
    if fcounter > eyeStructSize
        lastBin2Shift = 1000;
        eyestruct = eyestruct(end - lastBin2Shift + 1:end);
        eyestruct(lastBin2Shift:end) = repmat(eyestruct(end),eyeStructSize - lastBin2Shift,1);
        fcounter = lastBin2Shift + 1;
        disp('SHIFTBIN') % just for testing -  to see how often it is
    else
        fcounter = fcounter + 1;
    end
    
    %Adding new samples to eyestruct
    newsample = Eyelink('NewestFloatSample');
    if newsample.time ~= eyestruct(fcounter-1).time %Making sure we don't get redundant samples
        eyestruct(fcounter) = newsample;
    end
    
    
    whicheye = ~(eyestruct(fcounter).gx == invalid); %logical index of correct eye
    
    if sum(whicheye) < 1
        whicheye = 1:2 < 2; %Defaults to collecting from left eye if both have bad data
    end
    
    xcell = {eyestruct(1:fcounter).gx};
    ycell = {eyestruct(1:fcounter).gy};
    
    time = [eyestruct(1:fcounter).time];
    xgaze = cellfun(@(x) x(whicheye), xcell);
    ygaze = cellfun(@(x) x(whicheye), ycell);
    
    %cleaning up signal to let us tolerate blinks
    maybeblink = any(xgaze > 2000) && any(ygaze > 2000);
    if maybeblink
        ind_blink = xgaze > 2000 & ygaze > 2000;
        xgaze(ind_blink) = [];
        ygaze(ind_blink) = [];
        time(ind_blink) = []; %Applying same deletion to time vector
    end
    
    %Program cannot collect data as fast as Eyelink provides, so it's
    %necessary to check times for samples to get a good approximation
    %for how long a subject is fixating
    endtime = time(end);
    start_idx = find((time <= endtime - fixms), 1, 'last');
    
    if ~isempty(start_idx)
        lengthreq = length(start_idx:length(xgaze));
    else
        lengthreq = Inf;
    end
    
    if length(xgaze) >= lengthreq
        if all(xgaze(start_idx :end)  >= xbounds(1) & ...
                xgaze(start_idx :end) <= xbounds(2)) && ...
                all(ygaze(start_idx :end) >= ybounds(1) & ...
                ygaze(start_idx :end) <= ybounds(2))
            
            FixVal = 1;
            list{'Eyelink'}{'FixVal'} = 1;
            list{'Eyelink'}{'EyeStruct'} = eyestruct;
            Eyelink('Message', ['fixOn_' num2str(counter) '_' num2str(mglGetSecs)]);
        end
    end
end
end

function playstim(list)

%Adding current iteration to counter
counter = list{'Counter'}{'trial'};
questVersion = list{'meta'}{'questVersion'};

trialVarSequence = list{'Control'}{'trialVarSequence'};
freqType = trialVarSequence{counter};

if questVersion == 1
    switch freqType
        case 'H'
            q = list{'Quest'}{'ObjectH'};
        case 'L'
            q = list{'Quest'}{'ObjectL'};
    end
elseif questVersion == 2
    q = list{'Quest'}{'Object'};
end

ampTest = QuestQuantile(q);
ampTest = abs(ampTest);
hd = list{'Stimulus'}{'header'};

lastToneAmp = ampTest;

[~,waveform,f,h] = stimGen_noise_embedded_HL(hd.loFreq,hd.hiFreq,freqType,hd.toneDur,hd.toneIBI,0,0,lastToneAmp,hd.fs);

%importing important list objects
player = list{'Stimulus'}{'player'};
player.wave = waveform;
player.prepareToPlay;

preStimOnTime = mglGetSecs;

eyeTrackerOn = list{'Eyelink'}{'eyeTrackerOn'};
if eyeTrackerOn
    newsample = Eyelink('NewestFloatSample');
    Eyelink('Message', ['stimOn_' num2str(counter) '_' num2str(mglGetSecs)]); %Send timestamp to Eyelink before playing
end
player.play
postStimOnTime = mglGetSecs;

testAmps = list{'Stimulus'}{'testAmps'};
testAmps(counter) = ampTest;
list{'Stimulus'}{'testAmps'} = testAmps;

%Logging timestamps of the stimulus
stimStarts = list{'Timestamps'}{'stimStarts'};
stimStarts(counter) = player.playTime;
list{'Timestamps'}{'stimStarts'} = stimStarts;

waveforms = list{'Stimulus'}{'waveforms'};
waveforms{counter} = waveform;
list{'Stimulus'}{'waveforms'} = waveforms;

freq = list{'Stimulus'}{'freq'};
freq{counter} = f;
list{'Stimulus'}{'freq'} = freq;

isH = list{'Stimulus'}{'isH'};
isH(counter) = h;
list{'Stimulus'}{'isH'} = isH;

eyeTrackerOn = list{'Eyelink'}{'eyeTrackerOn'};
if eyeTrackerOn
    stimOnTimes = list{'Eyelink'}{'stimOnTimestamps'};
    stimOnTimes(counter) = newsample.time;
    list{'Eyelink'}{'stimOnTimestamps'} = stimOnTimes;
    
    preStimOnTimes = list{'Eyelink'}{'preStimOnTimestamps'};
    preStimOnTimes(counter)= preStimOnTime;
    list{'Eyelink'}{'preStimOnTimestamps'} = preStimOnTimes;
    
    postStimOnTimes = list{'Eyelink'}{'postStimOnTimestamps'};
    postStimOnTimes(counter)= postStimOnTime;
    list{'Eyelink'}{'postStimOnTimestamps'} = postStimOnTimes;
end
end

function string = waitForChoiceKey(list)
% Getting list items
counter = list{'Counter'}{'trial'};
ensemble = list{'Graphics'}{'ensemble'};
target = list{'Graphics'}{'target'};
ui = list{'Input'}{'Controller'};
player = list{'Stimulus'}{'player'};
stimStarts = list{'Timestamps'}{'stimStarts'};
responsewindow = list{'Input'}{'responseWindow'};

testAmps = list{'Stimulus'}{'testAmps'};
questVersion = list{'meta'}{'questVersion'};

isH = list{'Stimulus'}{'isH'}; % whether it's a high-freq trial
cur_f = isH(counter) + 1; % isH : 2 - high | 1 - low

timestamps = list{'Timestamps'}{'choices'};
choices = list{'Input'}{'choices'};
corrects = list{'Input'}{'corrects'};
RTs = list{'Input'}{'RT'};
eyeTrackerOn = list{'Eyelink'}{'eyeTrackerOn'};

ui.flushData

%Initializing variable
press = '';
rt = NaN;
choice = NaN;
cur_choice = NaN;
timestamp = NaN;

%Waiting for keypress

tic
while isempty(press)
    %Break loop if responsewindow expires and move to next trial
    if toc > responsewindow %This was previously Playsecs
        break
    end
    
    %Check for button press
    press = '';
    read(ui);
    [~, ~, eventname, ~] = ui.getHappeningEvent();
    if ~isempty(eventname) && length(eventname) == 1
        press = eventname;
        player.stop;        % once a response is detected, stop the stimulus
        
        
        if eyeTrackerOn
        newsample = Eyelink('NewestFloatSample');
        Eyelink('Message', ['response_' num2str(counter) '_' num2str(mglGetSecs)]); %Send timestamp to Eyelink once a response is reported

        postResponseTimestamp = mglGetSecs;

        %Get timestamp
        responseTimestamps = list{'Eyelink'}{'responseTimestamps'};
        responseTimestamps(counter) = newsample.time;
        list{'Eyelink'}{'responseTimestamps'} = responseTimestamps;
                
        postResponseTimestamps = list{'Eyelink'}{'postResponseTimestamps'};
        postResponseTimestamps(counter) = postResponseTimestamp;
        list{'Eyelink'}{'postResponseTimestamps'} = postResponseTimestamps;
        end
        %Get timestamp - stim_stop time
        stimStops = list{'Timestamps'}{'stimStops'};
        stimStops(counter) = player.stopTime;
        list{'Timestamps'}{'stimStops'} = stimStops;
    end
end

%Get timestamp - button press time
if ~isempty(press)
    timestamp = ui.history;
    timestamp = timestamp(timestamp(:, 2) > 1, :); %Just to make sure I get a timestamp from a pressed key/button
    timestamp = timestamp(end);
    rt = (timestamp - stimStarts(counter))*1000; %ms
    cur_choice = press{1};
end
timestamps(counter) = timestamp;
RTs(counter) = rt;

% check whether the choice was correct
if strcmp(cur_choice, 'right')
    choice = 2;
elseif strcmp(cur_choice, 'left')
    choice = 1;
end
choices(counter) = choice;

if cur_f == choice
    correct = 1;
    string = 'Correct';
else
    correct = 0;
    string = 'Incorrect';
end
corrects(counter) = correct;

if isH(counter)
    ensemble.setObjectProperty('yCenter', 5, target);
else
    ensemble.setObjectProperty('yCenter', -5, target);
end
ensemble.setObjectProperty('isVisible', true, target);

if questVersion == 1
    if isH(counter)
        q = list{'Quest'}{'ObjectH'};
        q = QuestUpdate(q, testAmps(counter), correct);
        list{'Quest'}{'ObjectH'} = q;
    else
        q = list{'Quest'}{'ObjectL'};
        q = QuestUpdate(q, testAmps(counter), correct);
        list{'Quest'}{'ObjectL'} = q;
    end
elseif questVersion == 2
    q = list{'Quest'}{'Object'};
    if isH(counter)
        q = QuestUpdate(q, testAmps(counter), choice-1);
    else
        q = QuestUpdate(q, -testAmps(counter), choice-1);
    end
    list{'Quest'}{'Object'} = q;
end

list{'Timestamps'}{'choices'} = timestamps;
list{'Input'}{'choices'} = choices;
list{'Input'}{'corrects'} = corrects;
list{'Input'}{'RT'} = RTs;

fprintf('Trial %d complete. Choice: %s (%s). RT: %3.3f \n', counter, cur_choice, string, rt);

% % print quest - for testing
% t=QuestMean(q);	
% sd=QuestSd(q);
% fprintf('Final threshold estimate (mean+-sd) is %.2f +- %.2f\n',t,sd);
% t=QuestMode(q);	
% fprintf('Mode threshold estimate is %4.2f\n',t);
% fprintf('Quest knew only your guess: %.2f +- %.2f.\n',q.tGuess,q.tGuessSd);
% 
% QuestBetaAnalysis(q); % optional
end


function showFeedback(list)
% hide the fixation point and cursor
ensemble = list{'Graphics'}{'ensemble'};
target = list{'Graphics'}{'target'};
counter = list{'Counter'}{'trial'};

% compare stimulus direction to choice direction
isCorrect = list{'Input'}{'corrects'};

% indicate correct or incorrect by coloring in the targets
if isnan(isCorrect(counter))
    ensemble.setObjectProperty('colors', list{'Graphics'}{'gray'}, target);
    feedback = list{'Feedback'}{'neg'};
    isCorrect(counter) = 0;
elseif isCorrect(counter)
    ensemble.setObjectProperty('colors', list{'Graphics'}{'green'}, target);
    feedback = list{'Feedback'}{'pos'};
else
    ensemble.setObjectProperty('colors', list{'Graphics'}{'red'}, target);
    feedback = list{'Feedback'}{'neg'};
end
feedback.prepareToPlay;
feedback.play;

list{'Input'}{'corrects'} = isCorrect;
end

function finishTrial(list)
ensemble = list{'Graphics'}{'ensemble'};
target = list{'Graphics'}{'target'};
ensemble.setObjectProperty('isVisible', false, target);

counter = list{'Counter'}{'trial'};
trialStops = list{'Timestamps'}{'trialStops'};
trialStops(counter) = mglGetSecs;
list{'Timestamps'}{'trialStops'} = trialStops;
eyeTrackerOn = list{'Eyelink'}{'eyeTrackerOn'};
if eyeTrackerOn
    trialStopTimestamps = list{'Eyelink'}{'trialStopTimestamps'};
    newsample = Eyelink('NewestFloatSample');
    Eyelink('Message', ['trialStop_' num2str(counter) '_' num2str(mglGetSecs)]);
    trialStopTimestamps(counter) = newsample.time;
    list{'Eyelink'}{'trialStopTimestamps'} = trialStopTimestamps;
end

pause(list{'timing'}{'intertrial'});
end


% function startsave(list)
%     data_folder = '/Research/uPenn_auditoryDecision/data/psychophysics/';
%     saveFilename = list{'meta'}{'saveFilename'};
%     save([data_folder saveFilename '_list.mat'], 'list');
% end