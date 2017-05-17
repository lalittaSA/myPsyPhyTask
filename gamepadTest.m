function [task, list] = gamepadTest(dispInd)

% 20170502: created by Lalitta - just to test gamepad

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

%% Setting up a list structure
list = topsGroupedList();

nTrials = 1;
list{'Counter'}{'nTrials'} = nTrials;

% Feedback 
pos_feedback = dotsPlayableFile();
pos_feedback.fileName = 'Coin.wav';
pos_feedback.intensity = 0.2;

list{'Feedback'}{'pos'} = pos_feedback;

%% time variables

list{'Timing'}{'responseWindow'} = 10;
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

%% Graphics

%Text prompts
leftprompt = dotsDrawableText();
leftprompt.string = 'press LEFT button';
leftprompt.fontSize = 42;
leftprompt.typefaceName = 'Calibri';
leftprompt.isVisible = false;

rightprompt = dotsDrawableText();
rightprompt.string = 'press RIGHT button';
rightprompt.fontSize = 42;
rightprompt.typefaceName = 'Calibri';
rightprompt.isVisible = false;

buttonprompt = dotsDrawableText();
buttonprompt.string = 'press A to quit';
buttonprompt.fontSize = 24;
buttonprompt.typefaceName = 'Calibri';
buttonprompt.isVisible = false;


%Graphical ensemble
ensemble = dotsEnsembleUtilities.makeEnsemble('drawables', isClient);
left = ensemble.addObject(leftprompt);
right = ensemble.addObject(rightprompt);
button = ensemble.addObject(buttonprompt);


list{'Graphics'}{'ensemble'} = ensemble;
list{'Graphics'}{'left'} = left;
list{'Graphics'}{'right'} = right;
list{'Graphics'}{'button'} = button;


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
    'Left', {show left},    {}, {@waitForLeftKey list}, 0, 'Right';
    'Right', {show right},  {}, {@waitForRightKey list}, 0, 'Finish'
    'Finish', {show button},{}, {@waitForCheckKey list}, 0, '';};
prepareMachine.addMultipleStates(prepStates);

list{'control'}{'prepareMachine'} = prepareMachine;


prepareConcurrents = topsConcurrentComposite();
prepareConcurrents.addChild(ensemble);
prepareConcurrents.addChild(prepareMachine);
prepareConcurrents.addChild(screen);

% add a branch to the tree trunk to lauch a Fixed Time trial
prepareTree = topsTreeNode();
prepareTree.addChild(prepareConcurrents);

% Top Level Runnables
task = topsTreeNode();
task.startFevalable = {@callObjectMethod, screen, @open};
task.finishFevalable = {@callObjectMethod, screen, @close};
task.addChild(prepareTree);
end

%% Accessory Functions

function waitForLeftKey(list)
% Getting list items
ui = list{'Input'}{'controller'};
ui.flushData;

%Initializing variable
press = '';

%Waiting for keypress
while ~strcmp(press, 'left')
    press = '';
    read(ui);
    [~, ~, eventname, ~] = ui.getHappeningEvent();
    if ~isempty(eventname) && length(eventname) == 1
        press = eventname;
    end
end

ensemble = list{'Graphics'}{'ensemble'};
left = list{'Graphics'}{'left'};
ensemble.setObjectProperty('isVisible', false, left);
end

function waitForRightKey(list)
% Getting list items
ui = list{'Input'}{'controller'};
ui.flushData;

%Initializing variable
press = '';

%Waiting for keypress
while ~strcmp(press, 'right')
    press = '';
    read(ui);
    [~, ~, eventname, ~] = ui.getHappeningEvent();
    if ~isempty(eventname) && length(eventname) == 1
        press = eventname;
    end
end

ensemble = list{'Graphics'}{'ensemble'};
right = list{'Graphics'}{'right'};
ensemble.setObjectProperty('isVisible', false, right);
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

ensemble = list{'Graphics'}{'ensemble'};
button = list{'Graphics'}{'button'};
ensemble.setObjectProperty('isVisible', false, button);
end



