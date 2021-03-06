function [task, list] = AudiDeci_noise_embedded_HL_cont_train(dispInd,ampRanges,subjId,optionName,eyeTrackerOn)

% 20170419: created by Lalitta - auditory decision task - 
% 2-alternative forced choice task: low-high frequency discrimination
% stimuli consist of series of tones embeded in noise in which 
% test tones with an amplitude that varies across trials are randomly interspersed
% stimulus-response combinations: high - left | low - right 
% using gamepade (L/R buttons) or keyboard (F/J keys; if a HID gamepad is not connected)

% eye tracker is also included

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

% eyeTrackerOn = 1;

%% Setting up the screen
sc = dotsTheScreen.theObject;
sc.reset('displayIndex', dispInd); %change display index to 0 for debug (small screen). 1 for full screen. Use >1 for external monitors.

%Call GetSecs just to load up the Mex files for getting time, so no delays later
GetSecs;

% get subject id
disp(['Starting main task: ' subjId]);
curDate = datestr(now,'yymmdd');
curTime = datestr(now,'HHMM');
curTask = mfilename;
saveFilename = [subjId '_' curDate '_' curTime '_' curTask '_' optionName];

%% Setting up a list structure
list = topsGroupedList(curTask);

% SUBJECT
list{'meta'}{'subjID'} = subjId;
list{'meta'}{'date'} = curDate;
list{'meta'}{'time'} = curTime;
list{'meta'}{'task'} = curTask;
list{'meta'}{'optionName'} = optionName;
list{'meta'}{'saveFilename'} = saveFilename;
list{'meta'}{'ampRanges'} = ampRanges;


%% call optionName to get settings 

optionFunctionName = ['Options_' curTask '_' optionName];
options = eval(optionFunctionName);

%% generate task conditions
list = AudiDeci_taskCondition_pretone(list,options);
nTrials = list{'Counter'}{'nTrials'};
%% fill audio settings in list
list{'Stimulus'}{'loFreq'} = options.loFreq;
list{'Stimulus'}{'hiFreq'} = options.hiFreq;
list{'Stimulus'}{'toneDur'} = options.toneDur;
list{'Stimulus'}{'toneIBI'} = options.toneIBI;
list{'Stimulus'}{'fs'} = options.fs;

%Creating audioplayer
player = dotsPlayableWave();
player.sampleFrequency = options.fs;
% player.duration = hd.trialDur/1000; %sec
player.intensity = 0.1;

% Feedback 
pos_feedback = dotsPlayableFile();
pos_feedback.fileName = 'Coin.wav';
pos_feedback.intensity = 0.2;
neg_feedback = dotsPlayableFile();
neg_feedback.fileName = 'beep-02.wav';
neg_feedback.intensity = 0.2;

end_player = dotsPlayableFile();
end_player.fileName = 'win.mp3';
end_player.intensity = 0.2;

list{'Feedback'}{'pos'} = pos_feedback;
list{'Feedback'}{'neg'} = neg_feedback;
list{'Feedback'}{'end_fx'} = end_player;

%% time variables

iti = 1;
list{'Timing'}{'intertrial'} = iti; %intertrial interval
list{'Timing'}{'responseWindow'} = options.responsewindow;
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

%% EYELINK  
list{'Eyelink'}{'eyeTrackerOn'} = eyeTrackerOn;

if eyeTrackerOn
list{'Eyelink'}{'FixAcq'} = 0.1;
list{'Eyelink'}{'FixHoldTime'} = 0.1;
list{'Eyelink'}{'SamplingFreq'} = 1000;

screensize = get(0, 'MonitorPositions');
screensize = screensize(1, [3, 4]);
centers = screensize/2;
list{'Eyelink'}{'Centers'} = centers;
list{'Eyelink'}{'Invalid'} = -32768;

%Setting windows for fixation:
window_width = 0.5*screensize(1);
window_height = 0.5*screensize(2);

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
list{'Eyelink'}{'FixBreaks'} = zeros(nTrials,1);
list{'Eyelink'}{'FixVal'} = 0;

end
%% add to the list

% COUNTER
list{'Counter'}{'trial'} = 0;

% STIMULUS
list{'Stimulus'}{'player'} = player;

list{'Stimulus'}{'waveforms'} = cell(nTrials,1);
list{'Stimulus'}{'freq'} = cell(nTrials,1);
list{'Stimulus'}{'isH'} = zeros(nTrials,1);

% TIMESTAMPS]
list{'Timestamps'}{'trialStarts'} = zeros(nTrials,1);
list{'Timestamps'}{'trialStops'} = zeros(nTrials,1);
list{'Timestamps'}{'stimStarts'} = zeros(nTrials,1);
list{'Timestamps'}{'stimStops'} = zeros(nTrials,1);
list{'Timestamps'}{'choices'} = zeros(nTrials,1);
list{'Timestamps'}{'rtOffset'} = zeros(nTrials,1);

% INPUT
list{'Input'}{'choices'} = zeros(nTrials,1);
list{'Input'}{'corrects'} = zeros(nTrials,1);
list{'Input'}{'RT'} = zeros(nTrials,1);


%% Graphics

list{'Graphics'}{'preCue width'} = 0.8;
list{'Graphics'}{'preCue height'} = 6;
list{'Graphics'}{'white'} = [1 1 1];
list{'Graphics'}{'gray'} = [0.5 0.5 0.5];
list{'Graphics'}{'red'} = [0.75 0.25 0.1];
list{'Graphics'}{'green'} = [.25 0.75 0.1];

%Text prompts
readyprompt = dotsDrawableText();
readyprompt.string = 'Calibration is finished!';
readyprompt.fontSize = 42;
readyprompt.typefaceName = 'Calibri';
readyprompt.isVisible = true;

buttonprompt = dotsDrawableText();
buttonprompt.string = 'press A to start the main task';
buttonprompt.fontSize = 24;
buttonprompt.typefaceName = 'Calibri';
buttonprompt.y = -2;
buttonprompt.isVisible = true;

% pre_cue object - a long vertical box with a horizontal line - line level indicates prior
preCue_box = dotsDrawableTargets();
preCue_box.xCenter = 0;
preCue_box.yCenter = 0;
preCue_box.nSides = 4;
preCue_box.colors = list{'Graphics'}{'gray'};
preCue_box.height = list{'Graphics'}{'preCue height'};
preCue_box.width = list{'Graphics'}{'preCue width'};
preCue_box.isVisible = false;

preCue_line = dotsDrawableLines();
% a horizontal line - length > box width
preCue_line.pixelSize = 3;
preCue_line.xFrom = preCue_box.xCenter-(preCue_box.width*3/4);
preCue_line.xTo = preCue_box.xCenter+(preCue_box.width*3/4);
% line level from prior - default value = 0
preCue_line.yFrom = 0;
preCue_line.yTo = 0;
preCue_line.isVisible = false;
preCue_line.colors = list{'Graphics'}{'white'};


list{'Graphics'}{'preCue_box'} = preCue_box;
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

%Text prompts
readyprompt2 = dotsDrawableText();
readyprompt2.string = 'Congratulations! Your performance is ';
readyprompt2.fontSize = 30;
readyprompt2.typefaceName = 'Calibri';
readyprompt2.isVisible = false;

buttonprompt2 = dotsDrawableText();
buttonprompt2.string = 'press A to quit';
buttonprompt2.fontSize = 24;
buttonprompt2.typefaceName = 'Calibri';
buttonprompt2.y = -2;
buttonprompt2.isVisible = false;

%Graphical ensemble
ensemble = dotsEnsembleUtilities.makeEnsemble('drawables', isClient);
box = ensemble.addObject(preCue_box);
line = ensemble.addObject(preCue_line);
target = ensemble.addObject(cursor);
ready = ensemble.addObject(readyprompt);
button = ensemble.addObject(buttonprompt);
ready2 = ensemble.addObject(readyprompt2);
button2 = ensemble.addObject(buttonprompt2);

list{'Graphics'}{'ensemble'} = ensemble;
list{'Graphics'}{'box'} = box;
list{'Graphics'}{'line'} = line;
list{'Graphics'}{'target'} = target;
list{'Graphics'}{'ready2'} = ready2;
list{'Graphics'}{'button2'} = button2;


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
list{'control'}{'trial calls'} = trialCalls;

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

list{'control'}{'prepareMachine'} = prepareMachine;

% State Machine, for use in maintask
mainMachine = topsStateMachine();
mainStates = {'name', 'entry', 'input', 'exit', 'timeout', 'next';
                 'CheckReady', {@startTrial list}, {}, {@waitForCheckKey list}, 0, 'Stimulus';
                 'Stimulus', {@playstim list}, {}, {}, 0, 'Choice';
                 'Choice', {@setGoTime list}, {}, {@waitForChoiceKey list}, 0, 'Feedback';
                 'Feedback', {@showFeedback list}, {}, {}, 0, 'Exit';
                 'Exit',{@finishTrial list}, {}, {}, iti,''};
mainMachine.addMultipleStates(mainStates);

list{'control'}{'mainMachine'} = mainMachine;

% End machine, for use in post-task
endMachine = topsStateMachine();
endStates = {'name', 'entry', 'input', 'exit', 'timeout', 'next';
    'Ready', {@startEndTask list},      {},      {@waitForCheckKey list},     0,       'Hide';
    'Hide', {hide [ready2 button2]}, {}, {}, 0, 'Finish';
    'Finish', {}, {}, {}, 0, '';};
endMachine.addMultipleStates(endStates);

list{'control'}{'endMachine'} = endMachine;

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

endConcurrents = topsConcurrentComposite();
endConcurrents.addChild(ensemble);
endConcurrents.addChild(endMachine);
endConcurrents.addChild(screen);

% add a branch to the tree trunk to lauch a Fixed Time trial
endTree = topsTreeNode();
endTree.addChild(endConcurrents);

% Top Level Runnables
task = topsTreeNode();
task.startFevalable = {@callObjectMethod, screen, @open};
task.finishFevalable = {@callObjectMethod, screen, @close};
task.addChild(prepareTree);
task.addChild(mainTree);
task.addChild(endTree);
end

%% Accessory Functions

function startTrial(list)
% clear data from the last trial
ui = list{'Input'}{'controller'};
ui.flushData();

counter = list{'Counter'}{'trial'};
counter = counter + 1;
list{'Counter'}{'trial'} = counter;

trialStarts = list{'Timestamps'}{'trialStarts'};
trialStarts(counter) = mglGetSecs;
list{'Timestamps'}{'trialStarts'} = trialStarts;

if list{'Eyelink'}{'eyeTrackerOn'}
    Eyelink('Message', ['trialStart_' num2str(counter) '_' num2str(mglGetSecs)]);
    trialStartTimestamps = list{'Eyelink'}{'trialStartTimestamps'};
    newsample = Eyelink('NewestFloatSample');
    trialStartTimestamps(counter) = newsample.time;
    list{'Eyelink'}{'trialStartTimestamps'} = trialStartTimestamps;
end

trialVarPrior = list{'Control'}{'trialVarPrior'};
ensemble = list{'Graphics'}{'ensemble'};
line = list{'Graphics'}{'line'};
box = list{'Graphics'}{'box'};

% set level of vertical line to prior level
yLevel = trialVarPrior(counter);
ensemble.setObjectProperty('yFrom', yLevel, line);
ensemble.setObjectProperty('yTo', yLevel, line);

ensemble.setObjectProperty('isVisible', true, box);
ensemble.setObjectProperty('isVisible', false, line);
ensemble.setObjectProperty('colors', list{'Graphics'}{'white'}, line);

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
if list{'Eyelink'}{'eyeTrackerOn'}
    list{'Eyelink'}{'FixVal'} = 0;
    checkFixation(list)
end

ensemble = list{'Graphics'}{'ensemble'};
line = list{'Graphics'}{'line'};
ensemble.setObjectProperty('isVisible', true, line);
end

function checkFixation(list)
fixtime = list{'Eyelink'}{'FixAcq'};
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
list{'Eyelink'}{'counter'} = fcounter;
end

function checkFixationHold(list)
%Import values
fs = list{'Eyelink'}{'SamplingFreq'};
fixtime = list{'Eyelink'}{'FixHoldTime'};
invalid = list{'Eyelink'}{'Invalid'};
xbounds = list{'Eyelink'}{'XBounds'};
ybounds = list{'Eyelink'}{'YBounds'};

counter = list{'Eyelink'}{'counter'};
eyestruct = list{'Eyelink'}{'EyeStruct'};

%Initializing the structure that temporarily holds eyelink sample data
%Ensuring eyestruct does not get prohibitively large.
%After 30 seconds it will shift the last second to the first and continue.
eyeStructSize = size(eyestruct,1);
fixms = fixtime*fs; %Getting number of fixated milliseconds needed

if counter > eyeStructSize
    lastBin2Shift = 1000;
    eyestruct = eyestruct(end - lastBin2Shift + 1:end);
    eyestruct(lastBin2Shift:end) = repmat(eyestruct(end),eyeStructSize - lastBin2Shift,1);
    counter = lastBin2Shift + 1;
else
    counter = counter + 1;
end

newsample = Eyelink('NewestFloatSample');
if newsample.time ~= eyestruct(counter-1).time %Making sure we don't get redundant samples
    eyestruct(counter) = newsample;
end

whicheye = ~(eyestruct(counter).gx == invalid); %logical index of correct eye

if sum(whicheye) < 1
    whicheye = 1:2 < 2; %Defaults to collecting from left eye if both have bad data
end

xcell = {eyestruct(1:counter).gx};
ycell = {eyestruct(1:counter).gy};
xgaze = cellfun(@(x) x(whicheye), xcell);
ygaze = cellfun(@(x) x(whicheye), ycell);

time = [eyestruct(1:counter).time];

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
    if all(xgaze(start_idx:end)  >= xbounds(1) & ...
            xgaze(start_idx:end) <= xbounds(2)) && ...
            all(ygaze(start_idx:end) >= ybounds(1) & ...
            ygaze(start_idx:end) <= ybounds(2))
        
        list{'Eyelink'}{'FixVal'} = 1;
        list{'Eyelink'}{'EyeStruct'} = eyestruct;
    else
        list{'Eyelink'}{'FixVal'} = 0;
        list{'Eyelink'}{'EyeStruct'} = eyestruct;
        ensemble = list{'Graphics'}{'ensemble'};
        line = list{'Graphics'}{'line'};
        ensemble.setObjectProperty('isVisible', false, line);
    end
end
end

function playstim(list)
counter = list{'Counter'}{'trial'};
rtOffset = list{'Timestamps'}{'rtOffset'};

trialVarStimSeq = list{'Control'}{'trialVarStimSeq'};
trialVarAmp = list{'Control'}{'trialVarAmp'};
ampRanges = list{'meta'}{'ampRanges'};

loFreq = list{'Stimulus'}{'loFreq'};
hiFreq = list{'Stimulus'}{'hiFreq'};
toneDur = list{'Stimulus'}{'toneDur'};
toneIBI = list{'Stimulus'}{'toneIBI'};
fs = list{'Stimulus'}{'fs'};
toneAmpH = ampRanges(2,2);
toneAmpL = ampRanges(1,2);
lastToneAmp = abs(trialVarAmp(counter));
freqType = trialVarStimSeq{counter};
tic
[~,waveform,f,h] = stimGen_noise_embedded_HL(loFreq,hiFreq,freqType,toneDur,toneIBI,toneAmpL,toneAmpH,lastToneAmp,fs);
toc
stimLength = length(trialVarStimSeq{counter});
rtOffset(counter) = ((stimLength-1) * toneDur) + ((stimLength-2) * toneIBI);
list{'Timestamps'}{'rtOffset'} = rtOffset;

%importing important list objects
player = list{'Stimulus'}{'player'};
player.wave = waveform;
player.prepareToPlay;

preStimOnTime = mglGetSecs;

if list{'Eyelink'}{'eyeTrackerOn'}
    Eyelink('Message', ['stimOn_' num2str(counter) '_' num2str(mglGetSecs)]); %Send timestamp to Eyelink before playing
    newsample = Eyelink('NewestFloatSample');
end
player.play
postStimOnTime = mglGetSecs;

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

if list{'Eyelink'}{'eyeTrackerOn'}
    stimOnTimes = list{'Eyelink'}{'stimOnTimestamps'};
    stimOnTimes(counter) = newsample.time;
    list{'Eyelink'}{'stimOnTimestamps'} = stimOnTimes;
    
    preStimOnTimes = list{'Eyelink'}{'preStimOnTimestamps'};
    preStimOnTimes(counter)= preStimOnTime;
    list{'Eyelink'}{'preStimOnTimestamps'} = preStimOnTimes;
    
    postStimOnTimes = list{'Eyelink'}{'postStimOnTimestamps'};
    postStimOnTimes(counter)= postStimOnTime;
    list{'Eyelink'}{'postStimOnTimestamps'} = postStimOnTimes;
    
    checkFixationHold(list)
    if ~list{'Eyelink'}{'FixVal'}
        fixBreaks = list{'Eyelink'}{'FixBreaks'};
        fixBreaks(counter) = 1;
        list{'Eyelink'}{'FixBreaks'} = fixBreaks;
    end
end
end

function setGoTime(list)
rtOffset = list{'Timestamps'}{'rtOffset'};
counter = list{'Counter'}{'trial'};
goTime = rtOffset(counter)/1000;
stimStarts = list{'Timestamps'}{'stimStarts'};
stimStart = stimStarts(counter);
curTime = mglGetSecs;
timeLapse = curTime - stimStart;
while timeLapse < goTime
    curTime = mglGetSecs;
    timeLapse = curTime - stimStart;
    if list{'Eyelink'}{'eyeTrackerOn'}
        checkFixationHold(list)
        if ~list{'Eyelink'}{'FixVal'}
            fixBreaks = list{'Eyelink'}{'FixBreaks'};
            fixBreaks(counter) = 1;
            list{'Eyelink'}{'FixBreaks'} = fixBreaks;
        end
    end
    ensemble = list{'Graphics'}{'ensemble'};
    line = list{'Graphics'}{'line'};
    ensemble.setObjectProperty('colors', list{'Graphics'}{'gray'}, line);
end

end

function string = waitForChoiceKey(list)
% Getting list items
ui              = list{'Input'}{'controller'};
counter         = list{'Counter'}{'trial'};
isH             = list{'Stimulus'}{'isH'}; % whether it's a high-freq trial
choices         = list{'Input'}{'choices'};
player          = list{'Stimulus'}{'player'};
target          = list{'Graphics'}{'target'};
ensemble        = list{'Graphics'}{'ensemble'};
rtOffset        = list{'Timestamps'}{'rtOffset'};
stimStarts      = list{'Timestamps'}{'stimStarts'};
responsewindow  = list{'Timing'}{'responseWindow'};

if list{'Eyelink'}{'eyeTrackerOn'}
    fixBreaks   = list{'Eyelink'}{'FixBreaks'};
end

%Initializing variable
ui.flushData
press       = '';
rt          = NaN;
choice      = NaN;
cur_choice  = NaN;
timestamp   = NaN;

%Waiting for keypress
tic
while isempty(press)
    if list{'Eyelink'}{'eyeTrackerOn'}
        checkFixationHold(list)
        if ~list{'Eyelink'}{'FixVal'} || fixBreaks(counter)

            player.stop;
            
            Eyelink('Message', ['stimOff_' num2str(counter) '_' num2str(mglGetSecs)]);
            stimStops = list{'Timestamps'}{'stimStops'};
            stimStops(counter) = player.stopTime;
            list{'Timestamps'}{'stimStops'} = stimStops;
            
            fixBreaks(counter) = 1;
            list{'Eyelink'}{'FixBreaks'} = fixBreaks;
            break
        end
    end
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
        
        if list{'Eyelink'}{'eyeTrackerOn'}
        Eyelink('Message', ['stimOff_' num2str(counter) '_' num2str(mglGetSecs)]);
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
    rt = ((timestamp - stimStarts(counter))*1000) - rtOffset(counter); %ms
    cur_choice = press{1};
end
cur_f = isH(counter) + 1; % isH : 2 - high | 1 - low

%Updating choices list
timestamps = list{'Timestamps'}{'choices'};
timestamps(counter) = timestamp;
list{'Timestamps'}{'choices'} = timestamps;

if strcmp(cur_choice, 'right')
    choice = 2;
elseif strcmp(cur_choice, 'left')
    choice = 1;
end
choices(counter) = choice;
list{'Input'}{'choices'} = choices;

% check whether the choice was correct
if isempty(press)
    correct = nan;
    string = 'Incorrect';
elseif cur_f == choice
    correct = 1;
    string = 'Correct';
else
    correct = 0;
    string = 'Incorrect';
end

if isH(counter)
    ensemble.setObjectProperty('yCenter', 5, target);
else
    ensemble.setObjectProperty('yCenter', -5, target);
end
ensemble.setObjectProperty('isVisible', true, target);


corrects = list{'Input'}{'corrects'};
corrects(counter) = correct;
list{'Input'}{'corrects'} = corrects;

RTs = list{'Input'}{'RT'};
RTs(counter) = rt;
list{'Input'}{'RT'} = RTs;

fprintf('Trial %d complete. Choice: %s (%s). RT: %3.3f \n', counter, cur_choice, string, rt);
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

if list{'Eyelink'}{'eyeTrackerOn'}
    Eyelink('Message', ['trialStop_' num2str(counter) '_' num2str(mglGetSecs)]);
    trialStopTimestamps = list{'Eyelink'}{'trialStopTimestamps'};
    newsample = Eyelink('NewestFloatSample');
    trialStopTimestamps(counter) = newsample.time;
    list{'Eyelink'}{'trialStopTimestamps'} = trialStopTimestamps;
end

% startsave(list) % never again! too slow
pause(list{'Timing'}{'intertrial'});
end

function startEndTask(list)
% overall performance
ensemble = list{'Graphics'}{'ensemble'};
line = list{'Graphics'}{'line'};
box = list{'Graphics'}{'box'};
ensemble.setObjectProperty('isVisible', false, line);
ensemble.setObjectProperty('isVisible', false, box);

end_fx = list{'Feedback'}{'end_fx'};
end_fx.prepareToPlay;
end_fx.play;

corrects = list{'Input'}{'corrects'};
perf = 100*sum(corrects)/length(corrects);

% prepare text + performance

ready2 = list{'Graphics'}{'ready2'};
button2 = list{'Graphics'}{'button2'};
tmp_str = ensemble.getObjectProperty('string', ready2);
tmp_str = [tmp_str num2str(perf) ' %'];
ensemble.setObjectProperty('string', tmp_str, ready2);

% make them visible
ensemble.setObjectProperty('isVisible', true, ready2);
ensemble.setObjectProperty('isVisible', true, button2);
end

function startsave(list)
    data_folder = './data/';
    saveFilename = list{'meta'}{'saveFilename'};
    save([data_folder saveFilename '_list.mat'], 'list');
end