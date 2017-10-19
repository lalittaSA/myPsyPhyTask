function [task, list] = AudiDeci_noise_embedded_HL(dispInd,ampRanges,subj_id)

% 20170322: created by Lalitta - auditory decision task - 
% 2-alternative forced choice task: low-high frequency discrimination
% stimuli consist of series of tones embeded in noise the last of which is
% a test tone with an amplitude that varies across trials
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

%% Setting up the screen
sc = dotsTheScreen.theObject;
sc.reset('displayIndex', dispInd); %change display index to 0 for debug (small screen). 1 for full screen. Use >1 for external monitors.

%Call GetSecs just to load up the Mex files for getting time, so no delays later
GetSecs;

% get subject id
% subj_id = input('Subject ID: ','s');
cur_date = datestr(now,'yymmdd');
cur_time = datestr(now,'HHMM');
cur_task = mfilename;
save_filename = [subj_id '_' cur_date '_' cur_time '_' cur_task];

%% Setting up a list structure
list = topsGroupedList(cur_task);

% SUBJECT
list{'meta'}{'subjID'} = subj_id;
list{'meta'}{'date'} = cur_date;
list{'meta'}{'time'} = cur_time;
list{'meta'}{'task'} = cur_task;
list{'meta'}{'saveFilename'} = save_filename;

%% settings for generating sequence of conditions

sequenceTypes = {'LLLLX','HLHLX'}; % from {'HHHHX','LLLLX','HLHLX','LHLHX'} | X: decision tone | all sequences must have the same length!
priorValues = [-2 0 2]; % from {-3 -2 -1 0 1 2 3} | prior setting (arbitrary numbers -> used to set location of pre-cue)

maxPriorPossible = 3;

% amplitude setting -> depending on prior : high prior -> more high-frequency trials
snrLevels = [-1 -0.5 -0.1 -0.01 0.01 0.1 0.5 1]; % minus - low | plus - high

minIntenseLo = ampRanges(1,1);
maxIntenseLo = ampRanges(1,2);
minIntenseHi = ampRanges(2,1);
maxIntenseHi = ampRanges(2,2);

ampLevels = snrLevels;
% set H amplitude
rangeH = maxIntenseHi - minIntenseHi;
ampLevels(ampLevels > 0) = (ampLevels(ampLevels > 0)*rangeH) + minIntenseHi;

% set L amplitude
rangeL = maxIntenseLo - minIntenseLo;
ampLevels(ampLevels < 0) = (ampLevels(ampLevels < 0)*rangeL) - minIntenseLo;

%% setting combinations of coherence levels for each prior level (7 presets)

sequenceLength = length(sequenceTypes{1});
nSequence = length(sequenceTypes);
nPrior = length(priorValues);
nSnr = length(snrLevels);

snrSetting = zeros(nPrior,nSnr);
snrValues = cell(nPrior,1);
ampValues = cell(nPrior,1);
for pp = 1:nPrior
    switch priorValues(pp) 
        % minus: more low-freq trials   %%% lenghts must match length(snrValues)
        case -3, snrSetting(pp,:) = [5 5 5 5 0 0 0 0]; % 5:0
        case -2, snrSetting(pp,:) = [4 4 4 4 1 1 1 1]; % 4:1
        case -1, snrSetting(pp,:) = [3 3 3 3 2 2 2 2]; % 3:2
        case  0, snrSetting(pp,:) = [2 2 3 3 3 3 2 2]; % 1:1
        case  1, snrSetting(pp,:) = [2 2 2 2 3 3 3 3]; % 2:3
        case  2, snrSetting(pp,:) = [1 1 1 1 4 4 4 4]; % 1:4
        case  3, snrSetting(pp,:) = [0 0 0 0 5 5 5 5]; % 0:5
        % plus: more high-freq trials
    end
    tmp_snr = [];
    tmp_amp = [];
    for cc = 1:nSnr
        if snrSetting(pp,cc) > 0
            tmp_snr = [tmp_snr repmat(snrLevels(cc),1,snrSetting(pp,cc))];
            tmp_amp = [tmp_amp repmat(ampLevels(cc),1,snrSetting(pp,cc))];
        end
    end
    snrValues{pp} = tmp_snr;
    ampValues{pp} = tmp_amp;
end


%% prior condition for each trial

pick_method = 'shuffledEach';
blockSize = 20;
blockRep = 5;
nCond = nSequence*nPrior;
nBlock = nCond*blockRep;
nTrials = nCond*blockRep*blockSize;

taskConditions = topsConditions(cur_task);

sequenceParameter = 'sequenceType';
taskConditions.addParameter(sequenceParameter, sequenceTypes);

priorParameter = 'priorLevel';
taskConditions.addParameter(priorParameter, num2cell(priorValues));

likesSequencePrior = topsFoundation();
taskConditions.addAssignment('sequenceType', likesSequencePrior, '.', 'name', '{}', {1});
taskConditions.addAssignment('priorLevel', likesSequencePrior, '.', 'name', '{}', {2});

switch pick_method
    case 'shuffledEach'
        taskConditions.setPickingMethod('shuffledEach',blockRep);
        taskConditions.run();
        condArray = cell(nCond,1);
        counter = 1;
        for ss = 1:nSequence
            for pp = 1:nPrior
                condArray{counter} = {sequenceTypes{ss},priorValues(pp)};
                counter = counter + 1;
            end
        end
        taskConds = cell(nBlock,1);
        for ii = 1:nBlock
            taskConds{ii} = condArray{taskConditions.pickSequence(ii)};
        end
    case 'coin-toss'
        taskConditions.setPickingMethod('coin-toss');
        taskConditions.maxPicks = nBlock;
        taskConds = cell(nBlock,1);
        keepGoing = true;
        counter = 1;
        while keepGoing
            taskConditions.run();
            taskConds{counter} = likesSequencePrior.name;
            keepGoing = ~taskConditions.isDone;
            counter = counter + 1;
        end
end

taskConds = repmat(taskConds,1,blockSize);
taskConds = reshape(taskConds',[],1);
taskCondTable = cell2table(taskConds);

trialVarSequence = taskCondTable.taskConds(:,1);
trialVarPrior = cell2mat(taskCondTable.taskConds(:,2));
list{'Control'}{'trialVarPreStim'} = trialVarSequence;

%% generate snrLevels for each priorLevel
trialVarSNR = zeros(size(trialVarPrior));
trialVarAmp = zeros(size(trialVarPrior));
for ss = 1:nSequence
    for pp = 1:nPrior
        ind = find(trialVarPrior == priorValues(pp) & strcmp(trialVarSequence,sequenceTypes{ss}));
        cur_snr_values = num2cell(snrValues{pp});
        cur_nT = length(ind);
        min_nT = length(snrValues{pp});
        n_rep = ceil(cur_nT/min_nT);
        
        % generate cohLevels
        tmp_snr = topsConditions('snr');
        tmp_snr.addParameter('snrLevel', cur_snr_values);
        likesSnrLevel = topsFoundation();
        tmp_snr.addAssignment('snrLevel',likesSnrLevel, '.', 'name');
        tmp_snr.setPickingMethod('shuffledEach',n_rep);
        
        trialVarSNR(ind) = (snrValues{pp}(tmp_snr.pickSequence(1:cur_nT)))';
        trialVarAmp(ind) = (ampValues{pp}(tmp_snr.pickSequence(1:cur_nT)))';
    end
end

% fill in last tone
for tt = 1:height(taskCondTable)
    switch sign(trialVarSNR(tt))
        case -1, taskCondTable.taskConds{tt,1}(sequenceLength) = 'L';
        case 1, taskCondTable.taskConds{tt,1}(sequenceLength) = 'H';
    end
end

trialVarSequence = taskCondTable.taskConds(:,1);

list{'Control'}{'trialVarSNR'} = trialVarSNR;
list{'Control'}{'trialVarAmp'} = trialVarAmp;
list{'Control'}{'taskConditions'} = taskConditions;
list{'Control'}{'taskCondTable'} = taskCondTable;
list{'Control'}{'trialVarSequence'} = trialVarSequence;
list{'Control'}{'trialVarPrior'} = trialVarPrior;
list{'Control'}{'maxPriorPossible'} = maxPriorPossible;
list{'Control'}{'sequenceLength'} = sequenceLength;

%% audio settings

hd.loFreq = 500; %hz      312.5 |  625 | 1250 | 2500 |  5000
hd.hiFreq = 2000; %hz     625   | 1250 | 2500 | 5000 | 10000 
hd.toneDur = 500; %ms 25 | 50
hd.toneIBI = 100; %ms  5 | 10

hd.fs = 44100;%384000;

%INPUT PARAMETERS
responsewindow = hd.toneDur/1000 + 3; %Time allowed to respond in, in seconds (== stim duration)
list{'Input'}{'responseWindow'} = responsewindow;

%Creating audioplayer
player = dotsPlayableWave();
player.sampleFrequency = hd.fs;
% player.duration = hd.trialDur/1000; %sec
player.intensity = 0.2;

% Feedback 
pos_feedback = dotsPlayableFile();
pos_feedback.fileName = 'Coin.wav';
pos_feedback.intensity = 0.4;
neg_feedback = dotsPlayableFile();
neg_feedback.fileName = 'beep-02.wav';
neg_feedback.intensity = 0.4;

end_player = dotsPlayableFile();
end_player.fileName = 'win.mp3';
end_player.intensity = 0.4;

list{'Feedback'}{'pos'} = pos_feedback;
list{'Feedback'}{'neg'} = neg_feedback;
list{'Feedback'}{'end_fx'} = end_player;

%% time variables

iti = 1;
list{'timing'}{'intertrial'} = iti; %intertrial interval
list{'timing'}{'preCue'} = 1;
list{'timing'}{'feedback'} = 1;
list{'timing'}{'rtOffset'} = hd.toneDur*(sequenceLength-1) + hd.toneIBI*(sequenceLength-1);

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

list{'Stimulus'}{'waveforms'} = cell(nTrials,1);
list{'Stimulus'}{'freq'} = cell(nTrials,1);
list{'Stimulus'}{'isH'} = zeros(nTrials,1);

% TIMESTAMPS]
list{'Timestamps'}{'stim_start'} = zeros(nTrials,1);
list{'Timestamps'}{'stim_stop'} = zeros(nTrials,1);
list{'Timestamps'}{'choices'} = zeros(nTrials,1);

% INPUT
list{'Input'}{'choices'} = zeros(nTrials,1);
list{'Input'}{'corrects'} = zeros(nTrials,1);
list{'Input'}{'RT'} = zeros(nTrials,1);


%% Graphics

list{'Graphics'}{'preCue width'} = 0.8;
list{'Graphics'}{'preCue height'} = 4;
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
preCue_box = cell(sequenceLength,1);
preCue_line = cell(sequenceLength,1);
for tt = 1:sequenceLength
    preCue_box{tt} = dotsDrawableTargets();
    preCue_box{tt}.xCenter = (tt - (sequenceLength+1)/2) * 3;
    preCue_box{tt}.yCenter = 0;
    preCue_box{tt}.nSides = 4;
    preCue_box{tt}.colors = list{'Graphics'}{'gray'};
    preCue_box{tt}.height = list{'Graphics'}{'preCue height'};
    preCue_box{tt}.width = list{'Graphics'}{'preCue width'};
    preCue_box{tt}.isVisible = false;
    
    preCue_line{tt} = dotsDrawableLines();
    % a horizontal line - length > box width
    preCue_line{tt}.pixelSize = 3;
    preCue_line{tt}.xFrom = preCue_box{tt}.xCenter-(preCue_box{tt}.width*3/4);
    preCue_line{tt}.xTo = preCue_box{tt}.xCenter+(preCue_box{tt}.width*3/4);
    % line level from prior - default value = 0
    preCue_line{tt}.yFrom = 0;
    preCue_line{tt}.yTo = 0;
    preCue_line{tt}.isVisible = false;
    if tt == sequenceLength
        preCue_line{tt}.colors = list{'Graphics'}{'white'};
    else
        preCue_line{tt}.colors = list{'Graphics'}{'gray'};
    end
end

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
box = zeros(1,sequenceLength);
line = zeros(1,sequenceLength);
for tt = 1:sequenceLength
    box(tt) = ensemble.addObject(preCue_box{tt});
    line(tt) = ensemble.addObject(preCue_line{tt});
end
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
                 'Stimulus', {@playstim list}, {}, {@waitForChoiceKey list}, 0, 'Feedback';
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

function startTrial(list)
% clear data from the last trial
ui = list{'Input'}{'controller'};
ui.flushData();

counter = list{'Counter'}{'trial'};
counter = counter + 1;
list{'Counter'}{'trial'} = counter;

trialVarPrior = list{'Control'}{'trialVarPrior'};
maxPriorPossible = list{'Control'}{'maxPriorPossible'};
trialVarSequence = list{'Control'}{'trialVarSequence'};
sequenceLength = list{'Control'}{'sequenceLength'};
ensemble = list{'Graphics'}{'ensemble'};
line = list{'Graphics'}{'line'};
precueHeight = list{'Graphics'}{'preCue height'};
sizescaling = precueHeight/(maxPriorPossible*2);

% set level of vertical line to prior level
for tt = 1:sequenceLength
    if tt == sequenceLength
        yLevel = trialVarPrior(counter)*sizescaling;
    else
        switch trialVarSequence{counter}(tt)
            case 'H', yLevel = precueHeight/2;
            case 'L', yLevel = -precueHeight/2;
        end
    end
    ensemble.setObjectProperty('yFrom', yLevel*3/4, line(tt));
    ensemble.setObjectProperty('yTo', yLevel*3/4, line(tt));
end

end

function playstim(list)

counter = list{'Counter'}{'trial'};
trialVarSNR = list{'Control'}{'trialVarSNR'};
trialVarSequence = list{'Control'}{'trialVarSequence'};

hd = list{'Stimulus'}{'header'};
toneAmpH = max(trialVarSNR);
toneAmpL = abs(min(trialVarSNR));
lastToneAmp = abs(trialVarSNR(counter));
freqType = trialVarSequence{counter};
[~,waveform,f,h] = stimGen_noise_embedded_HL(hd.loFreq,hd.hiFreq,freqType,hd.toneDur,hd.toneIBI,toneAmpL,toneAmpH,lastToneAmp,hd.fs);

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

freq = list{'Stimulus'}{'freq'};
freq{counter} = f;
list{'Stimulus'}{'freq'} = freq;

isH = list{'Stimulus'}{'isH'};
isH(counter) = h;
list{'Stimulus'}{'isH'} = isH;
end

function string = waitForChoiceKey(list)
% Getting list items
counter = list{'Counter'}{'trial'};
ensemble = list{'Graphics'}{'ensemble'};
target = list{'Graphics'}{'target'};
ui = list{'Input'}{'controller'};
player = list{'Stimulus'}{'player'};
stim_start = list{'Timestamps'}{'stim_start'};
responsewindow = list{'Input'}{'responseWindow'};
rtOffset = list{'timing'}{'rtOffset'};

choices = list{'Input'}{'choices'};

isH = list{'Stimulus'}{'isH'}; % whether it's a high-freq trial

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
        
        %Get timestamp - stim_stop time
        stim_stop = list{'Timestamps'}{'stim_stop'};
        stim_stop(counter) = player.stopTime;
        list{'Timestamps'}{'stim_stop'} = stim_stop;
    end
end

%Get timestamp - button press time
if ~isempty(press)
    timestamp = ui.history;
    timestamp = timestamp(timestamp(:, 2) > 1, :); %Just to make sure I get a timestamp from a pressed key/button
    timestamp = timestamp(end);
    rt = ((timestamp - stim_start(counter))*1000) - rtOffset; %ms
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
% startsave(list) % too slow

pause(list{'timing'}{'intertrial'});
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

% function startsave(list)
%     data_folder = '/Research/uPenn_auditoryDecision/data/psychophysics/';
%     save_filename = list{'meta'}{'save_filename'};
%     save([data_folder save_filename '_list.mat'], 'list');
% end