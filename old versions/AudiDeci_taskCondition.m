function list = AudiDeci_taskCondition(list,options)

%% get frequency settings
freqTypes = options.freqTypes;
freqNames = options.freqNames;
nRep = options.nRep;
%% get prior & amplitude settings and generate snr combinations for each prior level
priorLevels = options.priorLevels;
snrLevels = options.snrLevels;
snrPrior = options.snrPrior;

nPrior = length(priorLevels);
nSnr = length(snrLevels);

snrSetting = zeros(nPrior,nSnr);
snrValues = cell(nPrior,1);
for pp = 1:nPrior
    tmp_snrSetting = snrPrior(:,pp)';
    snrSetting(pp,:) = reshape(repmat(tmp_snrSetting,nSnr/2,1),1,nSnr);
    tmp_snr = [];
    for cc = 1:nSnr
        if snrSetting(pp,cc) > 0
            tmp_snr = [tmp_snr repmat(snrLevels(cc),1,snrSetting(pp,cc))];
        end
    end
    snrValues{pp} = tmp_snr;
end

blockSize = max(sum(snrSetting,2));

%% generate blocks of prior - random order

nBlock = nPrior * nRep;
nTrials = nPrior * blockSize * nRep;
pick_method = 'shuffledEach';
priorConditions = topsConditions();
priorParameter = 'priorLevel';
priorConditions.addParameter(priorParameter, num2cell(priorLevels));
likesPrior = topsFoundation();
priorConditions.addAssignment('priorLevel', likesPrior, '.', 'name');

switch pick_method
    case 'shuffledEach'
        priorConditions.setPickingMethod('shuffledEach',nRep);
        priorConditions.run();
        trialVarPrior = nan(nBlock,1);
        for ii = 1:nBlock
            trialVarPrior(ii) = priorLevels(priorConditions.pickSequence(ii));
        end
    case 'coin-toss'
        priorConditions.setPickingMethod('coin-toss');
        priorConditions.maxPicks = nBlock;
        trialVarPrior = nan(nBlock,1);
        keepGoing = true;
        counter = 1;
        while keepGoing || any(isnan(trialVarPrior))
            priorConditions.run();
            trialVarPrior(counter) = likesPrior.name;
            keepGoing = ~priorConditions.isDone;
            counter = counter + 1;
        end
end

trialVarPrior = repmat(trialVarPrior,1,blockSize);
trialVarPrior = reshape(trialVarPrior',[],1);

%% generate snrLevels & preToneSequence for each priorLevel
trialVarSNR = zeros(size(trialVarPrior));
trialVarPreToneSeq = cell(size(trialVarPrior));

for pp = 1:nPrior
    ind = find(trialVarPrior == priorLevels(pp));
    cur_nT = length(ind);
    min_nT = length(snrValues{pp});
    nRepPerPrior = cur_nT/min_nT;
    
    % generate cohLevels
    seqSnrCond = topsConditions();
    seqSnrCond.addParameter('snrLevel', num2cell(snrValues{pp}));

    
    likesSnr = topsFoundation();
    seqSnrCond.addAssignment('snrLevel',likesSnr,'.', 'name');
    seqSnrCond.setPickingMethod('shuffledEach',nRepPerPrior);
    
    keepGoing = true;
    counter = 1;
    while keepGoing
        seqSnrCond.run();
        trialVarSNR(ind(counter)) = likesSnr.name;
        trialVarPreToneSeq(ind(counter)) = {'X'};
        keepGoing = ~seqSnrCond.isDone;
        counter = counter + 1;
    end
end

%% fill in last tone of each trial depending on snr -> complete stim sequence
trialVarStimSeq = trialVarPreToneSeq;
for tt = 1:size(trialVarStimSeq,1)
    switch sign(trialVarSNR(tt))
        case -1, trialVarStimSeq{tt} = 'L';
        case 1, trialVarStimSeq{tt} = 'H';
    end
end

%% set amplitude according to snr & each subject's detection threshold
ampRanges = list{'meta'}{'ampRanges'};

ampLevels = snrLevels;
trialVarAmp = trialVarSNR;
minIntenseLo = ampRanges(1,1);
maxIntenseLo = ampRanges(1,2);
minIntenseHi = ampRanges(2,1);
maxIntenseHi = ampRanges(2,2);

% set H amplitude
rangeH = maxIntenseHi - minIntenseHi;
ampLevels(ampLevels > 0) = (ampLevels(ampLevels > 0)*rangeH) + minIntenseHi;

% set L amplitude
rangeL = maxIntenseLo - minIntenseLo;
ampLevels(ampLevels < 0) = (ampLevels(ampLevels < 0)*rangeL) - minIntenseLo;

for ss = 1:nSnr
    ind = trialVarSNR == snrLevels(ss);
    trialVarAmp(ind) = ampLevels(ss);
end
%% add to list
list{'Counter'}{'nTrials'} = nTrials;

list{'Control'}{'trialVarSNR'} = trialVarSNR;
list{'Control'}{'trialVarAmp'} = trialVarAmp;
list{'Control'}{'trialVarPrior'} = trialVarPrior;

list{'Control'}{'trialVarPreToneSeq'} = trialVarPreToneSeq;

list{'Control'}{'trialVarStimSeq'} = trialVarStimSeq;

