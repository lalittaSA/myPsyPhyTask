function list = AudiDeci_taskCondition(list,options)

%% get frequency settings
freqTypes = options.freqTypes;
freqNames = options.freqNames;

%% get pretone settings & generate pretone combinations
nPreTones = options.nPreTones;
preToneFreqs = options.preToneFreqs;

preToneSeq = cell(length(nPreTones),1);
preToneBias = cell(length(nPreTones),1);
preToneSeqNum = cell(length(nPreTones),1);

preToneSeqAll = [];
preToneBiasAll = [];
preToneLengthAll = [];
for pp = 1:length(nPreTones)
    switch preToneFreqs
        case 'single',  preToneSeqNum{pp} = repmat(freqTypes',1,nPreTones(pp));
        case 'combine', preToneSeqNum{pp} = combinator(length(freqNames),nPreTones(pp),'c','r');
        case 'permute', preToneSeqNum{pp} = combinator(length(freqNames),nPreTones(pp),'p','r');
    end
    preToneSeq{pp} = freqNames(preToneSeqNum{pp});
    preToneSeqAll = [preToneSeqAll;mat2cell([cell2mat(preToneSeq{pp}),repmat('X',size(preToneSeqNum{pp},1),1)],ones(size(preToneSeqNum{pp},1),1),size(preToneSeqNum{pp},2)+1)];
    preToneBias{pp} = sum(((preToneSeqNum{pp}-1)*2)-1,2)/size(preToneSeqNum{pp},2); % ratio L(neg)-H(pos) preTones | range = [-1,1];
    preToneBiasAll = [preToneBiasAll;preToneBias{pp}]; 
    preToneLengthAll = [preToneLengthAll;repmat(nPreTones(pp),size(preToneSeqNum{pp},1),1)];
end

% next 2 lines just to check bias type distribution
% preToneBiasEdges = [-1,-0.81,-0.61,-0.41,-0.21,-0.11,0.11,0.21,0.41,0.61,0.81,1];
% preToneBiasBins = histcounts(preToneBiasAll,preToneBiasEdges);

preToneBiasType = unique(preToneBiasAll);
nPreToneSeqType = size(preToneSeqAll,1);
%% get prior & amplitude settings and generate snr combinations for each prior level
priorLevels = options.priorLevels;
snrLevels = options.snrLevels;
snrPrior = options.snrPrior;

nPrior = length(priorLevels);
nSnr = length(snrLevels);

snrSetting = zeros(nPrior,nSnr);
snrValues = cell(nPrior,1);
for pp = 1:nPrior
    tmp_snrSetting = snrPrior(:,pp);
    snrSetting(pp,:) = reshape(repmat(tmp_snrSetting,nSnr/2,1),1,nSnr);
    tmp_snr = [];
    for cc = 1:nSnr
        if snrSetting(pp,cc) > 0
            tmp_snr = [tmp_snr repmat(snrLevels(cc),1,snrSetting(pp,cc))];
        end
    end
    snrValues{pp} = tmp_snr;
end

%% generate blocks of prior - random order
nRep = options.nRep;

blockSize = nPreToneSeqType * max(sum(snrSetting,2));
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
    cur_snr_values = num2cell(snrValues{pp});
    cur_nT = length(ind);
    min_nT = length(snrValues{pp})*nPreToneSeqType;
    nRepInBlock = ceil(cur_nT/min_nT);
    
    % generate cohLevels
    seqSnrCond = topsConditions();
    seqSnrCond.addParameter('snrLevel', cur_snr_values);
    seqSnrCond.addParameter('preToneLevel', preToneSeqAll);
    likesSeqAndAnr = topsFoundation();
    seqSnrCond.addAssignment('snrLevel',likesSeqAndAnr, '.', 'name','{}',{1});
    seqSnrCond.addAssignment('preToneLevel',likesSeqAndAnr, '.', 'name','{}',{2});
    seqSnrCond.setPickingMethod('shuffledEach',nRepInBlock);
    
    keepGoing = true;
    counter = 1;
    while keepGoing
        seqSnrCond.run();
        trialVarSNR(counter) = likesSeqAndAnr.name{1};
        trialVarPreToneSeq(counter) = likesSeqAndAnr.name(2);
        keepGoing = ~seqSnrCond.isDone;
        counter = counter + 1;
    end
end

%% feed out more information about pretone conditions - pretone length & bias (H-L ratio in pretones)
trialVarPreToneBias = zeros(size(trialVarPreToneSeq));
trialVarPreToneLength = zeros(size(trialVarPreToneSeq));
% add preTone information
for tt = 1:nPreToneSeqType
    ind = ismember(trialVarPreToneSeq,preToneSeqAll(tt));
    trialVarPreToneBias(ind) = preToneBiasAll(tt);
    trialVarPreToneLength(ind) = preToneLengthAll(tt);
end

%% fill in last tone of each trial depending on snr -> complete stim sequence
trialVarStimSeq = trialVarPreToneSeq;
for tt = 1:size(trialVarPreToneSeq,1)
    switch sign(trialVarSNR(tt))
        case -1, trialVarStimSeq{tt}(end) = 'L';
        case 1, trialVarStimSeq{tt}(end) = 'H';
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

list{'Control'}{'trialVarPreTone'} = trialVarPreToneSeq;
list{'Control'}{'trialVarPreToneBias'} = trialVarPreToneBias;
list{'Control'}{'trialVarPreToneLength'} = trialVarPreToneLength;

list{'Control'}{'trialVarStimSeq'} = trialVarStimSeq;

