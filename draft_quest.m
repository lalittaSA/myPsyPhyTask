%QUEST OBJECT
% Creating Quest structure
tGuess = 0.2; %Guess at the appropriate difference between intensities
tGuessSd = 3; %Standard deviation in guesses permitted

pThreshold = 0.70; %How successful do you want a subject to be?

beta = 3; % 3.5
delta = 0.01;
gamma = 0.5;

q = QuestCreate(tGuess, tGuessSd, pThreshold, beta, delta, gamma); %Quest object created

q.normalizePdf=1;

Machine = topsStateMachine();
stimList = {'name', 'entry', 'input', 'exit', 'timeout', 'next';
    'CheckReady', {}, {}, {@checkReady list}, 0, 'Stimulus';
    'Stimulus', {@checkquest list}, {}, {}, 0, 'CheckFix';
    'CheckFix', {@checkFixation list}, {}, {}, 0, 'Feedback';
    'Feedback', {}, {@queststim list}, {}, 0, '';
    'Correct', {despin dot}, {}, {}, 0, '';
    'Incorrect', {spin dot}, {}, {}, 0, ''};


trialsDesired=40;
wrongRight={'wrong','right'};
timeZero=GetSecs;
for k=1:trialsDesired
	% Get recommended level.  Choose your favorite algorithm.
	tTest=QuestQuantile(q);	% Recommended by Pelli (1987), and still our favorite.
	% 	tTest=QuestMean(q);		% Recommended by King-Smith et al. (1994)
	% 	tTest=QuestMode(q);		% Recommended by Watson & Pelli (1983)
	
	% We are free to test any intensity we like, not necessarily what Quest suggested.
	% 	tTest=min(-0.05,max(-3,tTest)); % Restrict to range of log contrasts that our equipment can produce.
	
	% Simulate a trial
	timeSplit=eval(getSecsFunction); % Omit simulation and printing from the timing measurements.
 	response=QuestSimulate(q,tTest,tActual);
 	fprintf('Trial %3d at %5.2f is %s\n',k,tTest,char(wrongRight(response+1)));
	timeZero=timeZero+eval(getSecsFunction)-timeSplit;
	
	% Update the pdf
	q=QuestUpdate(q,tTest,response); % Add the new datum (actual test intensity and observer response) to the database.
end



% Print results of timing.
fprintf('%.0f ms/trial\n',1000*(eval(getSecsFunction)-timeZero)/trialsDesired);

% Ask Quest for the final estimate of threshold.
t=QuestMean(q);		% Recommended by Pelli (1989) and King-Smith et al. (1994). Still our favorite.
sd=QuestSd(q);
fprintf('Final threshold estimate (mean+-sd) is %.2f +- %.2f\n',t,sd);
% t=QuestMode(q);	% Similar and preferable to the maximum likelihood recommended by Watson & Pelli (1983). 
% fprintf('Mode threshold estimate is %4.2f\n',t);
fprintf('\nYou set the true threshold to %.2f.\n',tActual);
fprintf('Quest knew only your guess: %.2f +- %.2f.\n',tGuess,tGuessSd);

% Optionally, reanalyze the data with beta as a free parameter.
fprintf('\nBETA. Many people ask, so here''s how to analyze the data with beta as a free\n');
fprintf('parameter. However, we don''t recommend it as a daily practice. The data\n');
fprintf('collected to estimate threshold are typically concentrated at one\n');
fprintf('contrast and don''t constrain beta. To estimate beta, it is better to use\n');
fprintf('100 trials per intensity (typically log contrast) at several uniformly\n');
fprintf('spaced intensities. We recommend using such data to estimate beta once,\n');
fprintf('and then using that beta in your daily threshold meausurements. With\n');
fprintf('that disclaimer, here''s the analysis with beta as a free parameter.\n');
QuestBetaAnalysis(q); % optional
fprintf('Actual parameters of simulated observer:\n');
fprintf('logC	beta	gamma\n');
fprintf('%5.2f	%4.1f	%5.2f\n',tActual,q.beta,q.gamma);



function string = checkquest(list)
string = 'Incorrect'; %Defaulting string to incorrect.

%import important list objects
counter = list{'Stimulus'}{'Counter'};
reactionwindow = list{'Input'}{'ReactionWindow'};
pattern = list{'Input'}{'ResponsePattern'};
ui = list{'Input'}{'Controller'};
playtimes = list{'Stimulus'}{'Playtimes'};

%Important objects pertinent to quest updating
q = list{'Quest'}{'Object'};
opposite_input_on = list{'Input'}{'OppositeOn'};
freqlist = list{'Stimulus'}{'Playfreqs'};
oddf = list{'Stimulus'}{'OddFreq'};
standardf = list{'Stimulus'}{'StandardFreq'};

%get current (playtime) and (playtime + reactionwindow)
playtime = playtimes(counter);
stoptime = playtime + reactionwindow;

%Get all ui.history rows between these times(inclusive)
history = ui.history(ui.history(:,3) >= playtime & ui.history(:,3) <= stoptime, :);

%get only rows where there are presses
history = history(history(:,2) > 1, :);

%patternmatch in for loop
width = length(pattern); %width of pattern expected, in samples
isPattern = 0;

%Check if the second column contains a matching pattern
responsetime = -1; %Initialize responsetime as nonsense for nonresponse trials
loc_vals = history(:,2);
for i = 1:length(loc_vals)-(width-1)
    loc_idx = i : i+(width-1);
    grouping = loc_vals(loc_idx)';
    isPattern = all(grouping == pattern);
    
    if isPattern
        responsetime = history(loc_idx(end),3); %Getting time of last button press in pattern
        break %then exiting loop
    end
end

%Checking correct
if opposite_input_on == 1
    checkfreq = standardf; %If opposite input is on, subjects must press button for standard frequency
else
    checkfreq = oddf;
end


if isPattern && freqlist(counter) == checkfreq %If they pressed button, was it a good press?
    correct = 1;
    string = 'Correct';
elseif ~isPattern && freqlist(counter) ~= checkfreq %Did they avoid pressing for the right reasons?
    correct = 1;
    string = 'Correct';
else
    correct = 0;
end


%QUEST UPDATER
% Getting actual difference between oddf and standardf
diff = oddf - standardf;

%Update QUEST object with latest response (the 'correct' variable)
q = QuestUpdate(q, diff, correct);
list{'Quest'}{'Object'} = q;

%Storing user input and timestamps
choices = list{'Input'}{'Choices'};
choices(counter) = isPattern;
list{'Input'}{'Choices'} = choices;

corrects = list{'Input'}{'Corrects'};
corrects(counter) = correct;
list{'Input'}{'Corrects'} = corrects;

responsetimes = list{'Timestamps'}{'Response'};
responsetimes(counter)= responsetime;
list{'Timestamps'}{'Response'} = responsetimes;

%Debug Display
fprintf('Trial %d complete. \n', counter);
end

function queststim(list)
%Adding current iteration to counter
counter = list{'Stimulus'}{'Counter'};
counter = counter + 1;
list{'Stimulus'}{'Counter'} = counter;

%Import important objects
player = list{'Stimulus'}{'Player'};
standardf = list{'Stimulus'}{'StandardFreq'};
p_odd = list{'Stimulus'}{'ProbabilityOdd'};
q = list{'Quest'}{'Object'};

%Formulate new oddf
freqdiff = abs(QuestQuantile(q));
oddf = standardf + freqdiff;
list{'Stimulus'}{'OddFreq'} = oddf;

%Dice roll to decide if odd or standard
roll = rand;
frequency(roll > p_odd) = standardf;
frequency(roll <= p_odd) = oddf;

%Prepping player and playing
player.frequency = frequency;
player.prepareToPlay;

playtime = mglGetSecs;
player.play;
Eyelink('Message', num2str(mglGetSecs)); %Send timestamp to Eyelink before playing

%Logging times and frequencies
playtimes = list{'Stimulus'}{'Playtimes'};
playtimes(counter) = playtime;
list{'Stimulus'}{'Playtimes'} = playtimes;

playfreqs = list{'Stimulus'}{'Playfreqs'};
playfreqs(counter) = frequency;
list{'Stimulus'}{'Playfreqs'} = playfreqs;
end
