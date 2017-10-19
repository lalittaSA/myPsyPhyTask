function list = AudiDeci_taskCondition_pretone_hazard(list,options)

%% get frequency settings
freqTypes = options.freqTypes;
freqNames = options.freqNames;

%% get pretone settings & generate pretone combinations
nPreTones = options.nPreTones;
preToneCombination = options.preToneCombination;

preToneSeq = cell(length(nPreTones),1);
preToneBias = cell(length(nPreTones),1);
preToneSeqNum = cell(length(nPreTones),1);

preToneSeqAll = [];
preToneBiasAll = [];
preToneLengthAll = [];
for pp = 1:length(nPreTones)
    switch preToneCombination
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

preToneBiasAll(isnan(preToneBiasAll)) = 0;
% next 2 lines just to check bias type distribution
% preToneBiasEdges = [-1,-0.81,-0.61,-0.41,-0.21,-0.11,0.11,0.21,0.41,0.61,0.81,1];
% preToneBiasBins = histcounts(preToneBiasAll,preToneBiasEdges);

preToneBiasType = unique(preToneBiasAll(preToneLengthAll~=0));
if isempty(preToneBiasType)
    preToneBiasType = [0;0];
end
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
    snrSetting(pp,:) = reshape(repmat(tmp_snrSetting',nSnr/2,1),1,nSnr);
    tmp_snr = [];
    for cc = 1:nSnr
        if snrSetting(pp,cc) > 0
            tmp_snr = [tmp_snr repmat(snrLevels(cc),1,snrSetting(pp,cc))];
        end
    end
    snrValues{pp} = tmp_snr;
end

nRep = options.nRep;
blockSize = max(sum(snrSetting,2));
nBlock = nPrior * nRep;

%% if hazard
if length(nPreTones) > 1;
    multiplier = 5;
else
    multiplier = 1;
end
nCond = nBlock * blockSize * multiplier;%length(nPreTones);
expDecay = exp(-nPreTones/5); % to get mean pretone lenght ~5
expDecay(nPreTones == 0) = expDecay(nPreTones == 0)/3;
%     plot(expDecay); % just for testing

nPreToneDistrib = nCond*expDecay/sum(expDecay);
sumDistrib = sum(nPreToneDistrib);

nPreToneDistrib = round(nPreToneDistrib*(nCond/sumDistrib));
sumDistrib = sum(nPreToneDistrib);
while sumDistrib > nCond
    nPreToneDistrib(1) = nPreToneDistrib(1) - 1;
    sumDistrib = sum(nPreToneDistrib);
end
while sumDistrib < nCond
    nPreToneDistrib(1) = nPreToneDistrib(1) + 1;
    sumDistrib = sum(nPreToneDistrib);
end

preToneLengthEdges = [-0.5,0.5,1.5,2.5,3.5,4.5,6.5,9.5,14.5];
% group pretone lengths to evenly scatter long pretones in all conditions (as the longer ones are rare)
preToneLengthList = [];
preToneSeqAll_hazard = [];
for nn = 1:length(nPreTones)
    preToneLengthList = [preToneLengthList;repmat(nPreTones(nn),nPreToneDistrib(nn),1)];
    for ff = 1:length(freqTypes)
        preToneSeqAll_hazard = [preToneSeqAll_hazard;repmat(preToneSeqAll((length(freqTypes)*(nn-1))+ff),nPreToneDistrib(nn),1)];
    end
end

preToneLengthCounts = histcounts(preToneLengthList,preToneLengthEdges);
preToneLengthNum = discretize(preToneLengthList,preToneLengthEdges);
mid_bins = preToneLengthEdges(1:end-1)'+(preToneLengthEdges(2:end)'-preToneLengthEdges(1:end-1)')/2;
preToneLengthGroups = mid_bins(preToneLengthNum);
preToneGroupLabel = unique([preToneLengthNum,preToneLengthGroups,preToneLengthList],'rows');

pretoneLengthGroup_all = repmat(preToneLengthNum,1,length(freqTypes));
pretoneLengthGroup_all = reshape(pretoneLengthGroup_all',[],1);
pretoneBias_all = repmat(preToneBiasType,nCond,1);

preToneLengthType = unique(preToneLengthNum);
%% generate blocks of prior - random order
nTrials = sumDistrib * length(freqTypes);
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

trialVarPrior = repmat(trialVarPrior,1,nTrials/nBlock);
trialVarPrior = reshape(trialVarPrior',[],1);

%% generate snrLevels & preToneSequence for each priorLevel
% trialVarSNR = zeros(size(trialVarPrior));
% trialVarPreToneSeq = cell(size(trialVarPrior));
% for pp = 1:nPrior
%     ind = find(trialVarPrior == priorLevels(pp));
%     cur_snr_values = num2cell(snrValues{pp});
%     cur_nT = length(ind);
%     min_nT = length(snrValues{pp})*nPreToneSeqType;
%     nRepInBlock = ceil(cur_nT/min_nT);
%     
%     % generate cohLevels
%     seqSnrCond = topsConditions();
%     seqSnrCond.addParameter('snrLevel', cur_snr_values);
%     seqSnrCond.addParameter('preToneLevel', preToneSeqAll_hazard);
%     likesSeqAndAnr = topsFoundation();
%     seqSnrCond.addAssignment('snrLevel',likesSeqAndAnr, '.', 'name','{}',{1});
%     seqSnrCond.addAssignment('preToneLevel',likesSeqAndAnr, '.', 'name','{}',{2});
%     seqSnrCond.setPickingMethod('coin-toss');
%     seqSnrCond.maxPicks = cur_nT;
% %     seqSnrCond.setPickingMethod('shuffledEach',nRepInBlock);
%     
%     keepGoing = true;
%     counter = 1;
%     while keepGoing
%         seqSnrCond.run();
%         trialVarSNR(counter) = likesSeqAndAnr.name{1};
%         trialVarPreToneSeq(counter) = likesSeqAndAnr.name(2);
%         keepGoing = ~seqSnrCond.isDone;
%         counter = counter + 1;
%     end
% end

trialVarSNR = zeros(size(trialVarPrior));
trialVarPreToneSeq = cell(size(trialVarPrior));
trialVarPreToneBias = zeros(size(trialVarPreToneSeq));
trialVarPreToneLength = zeros(size(trialVarPreToneSeq));

preToneLengthIndList = 1:length(preToneSeqAll_hazard);

for pp = 1:nPrior
    ind = find(trialVarPrior == priorLevels(pp));
    cur_nT = length(ind);
    min_nT = length(snrValues{pp})*length(preToneLengthType)*length(preToneBiasType);
    nRepPerPrior = ceil(cur_nT/min_nT);
    
    % generate cohLevels
    seqSnrCond = topsConditions();
    seqSnrCond.addParameter('snrLevel', num2cell(snrValues{pp}));
    seqSnrCond.addParameter('preToneBias', num2cell(preToneBiasType));
    seqSnrCond.addParameter('preToneLength', num2cell(preToneLengthType));
    
    likesSeqAndSnr = topsFoundation();
    seqSnrCond.addAssignment('snrLevel',likesSeqAndSnr, '.', 'name','{}',{1});
    seqSnrCond.addAssignment('preToneBias',likesSeqAndSnr, '.', 'name','{}',{2});
    seqSnrCond.addAssignment('preToneLength',likesSeqAndSnr, '.', 'name','{}',{3});
    seqSnrCond.setPickingMethod('shuffledEach',nRepPerPrior);
    
    keepGoing = true;
    counter = 1;
    while keepGoing && counter <= length(ind)
        seqSnrCond.run();
        trialVarSNR(ind(counter)) = likesSeqAndSnr.name{1};
        tmpBiasInd = likesSeqAndSnr.name{2};
        tmpLegnthGroupInd = likesSeqAndSnr.name{3};
        cur_sel = intersect(find(pretoneLengthGroup_all == tmpLegnthGroupInd & pretoneBias_all == tmpBiasInd),preToneLengthIndList);
        while isempty(cur_sel)
            remainLength = pretoneLengthGroup_all(preToneLengthIndList);
            takeOneLenght = randperm(length(remainLength));
            tmpLegnthGroupInd = remainLength(takeOneLenght(1));
            remainBias = pretoneBias_all(preToneLengthIndList);
            takeOneBias = randperm(length(remainBias));
            tmpBiasInd = remainBias(takeOneBias(1));
            cur_sel = intersect(find(pretoneLengthGroup_all == tmpLegnthGroupInd & pretoneBias_all == tmpBiasInd),preToneLengthIndList);
        end
        tmp_rand = randperm(length(cur_sel));
        cur_sel = cur_sel(tmp_rand(1));
        trialVarPreToneSeq(ind(counter)) = preToneSeqAll_hazard(cur_sel);
        % more information about pretone conditions
        trialVarPreToneBias(ind(counter)) = pretoneBias_all(cur_sel);
%         trialVarPreToneLength(ind(counter)) = pretoneLengthGroup_all(cur_sel);
%         trialVarLastConsPretone(ind(counter)) = nLastConsPretone(cur_sel);
        preToneLengthIndList(preToneLengthIndList == cur_sel) = [];
        keepGoing = ~seqSnrCond.isDone;
        counter = counter + 1;
    end
end
%% feed out more information about pretone conditions - pretone length & bias (H-L ratio in pretones)
trialVarPreToneLength = zeros(size(trialVarPreToneSeq));
trialVarLastConsPretone = zeros(size(trialVarPreToneSeq));

for tt = 1:size(trialVarPreToneSeq,1)
    curPretoneType = trialVarPreToneSeq{tt}(1);
    curPreToneLength = length(trialVarPreToneSeq{tt})-1;
    % if flip (5% probability of frequency flip in the pre-tones)
    if options.pretoneFlip
        if curPreToneLength > 1
            curRand = rand(1,curPreToneLength);
            curFlip = curRand < options.pretoneFlipProb;
            if any(curFlip) % change ONLY ONE pretone to opposite frequency
                nFlip = sum(curFlip);
                if nFlip > 1
                    tmpRandPerm = randperm(nFlip);
                    indFlip = find(curFlip);
                    curFlip(indFlip(tmpRandPerm(2:end))) = 0;
                end
                switch curPretoneType
                    case 'H', trialVarPreToneSeq{tt}(curFlip) = 'L';
                    case 'L', trialVarPreToneSeq{tt}(curFlip) = 'H';
                end
            end
        end
    end
    % update preTone information
    if curPreToneLength > 1
        trialVarPreToneBias(tt) = (sum(trialVarPreToneSeq{tt} == 'H') - sum(trialVarPreToneSeq{tt} == 'L'))/curPreToneLength;
    else
        trialVarPreToneBias(tt) = 0;
    end
    trialVarPreToneLength(tt) = curPreToneLength;
    curChangeReversePreTone = sign(diff(fliplr(trialVarPreToneSeq{tt}(1:end-1)))); %H->L:1 L->H:-1
    if any(curChangeReversePreTone)
        indFirstChangeReverse = find(curChangeReversePreTone ~= 0, 1);
        trialVarLastConsPretone(tt) = indFirstChangeReverse * curChangeReversePreTone(indFirstChangeReverse); 
    else
        switch curPretoneType
            case 'H', trialVarLastConsPretone(tt) = curPreToneLength;
            case 'L', trialVarLastConsPretone(tt) = -curPreToneLength;
        end
    end
end

%% fill in last tone of each trial depending on snr -> complete stim sequence
trialVarStimSeq = trialVarPreToneSeq;
for tt = 1:size(trialVarPreToneSeq,1)
    switch sign(trialVarSNR(tt))
        case -1, trialVarStimSeq{tt}(end) = 'L';
        case 1, trialVarStimSeq{tt}(end) = 'H';
    end
end

%% just for testing condition distribution
% priorList = unique(trialVarPrior);
% for pp = 1:length(priorList)
%     selTrials = trialVarPrior == priorList(pp);
%     selDist = histc(trialVarSNR(selTrials),-1.5:0.1:1.5);
%     plot(-1.5:0.1:1.5,selDist)
%     hold on
% end
% 
% mean(trialVarPreToneLength);

%% set amplitude according to snr & each subject's detection threshold
ampLevels = snrLevels;
trialVarAmp = trialVarSNR;
minIntenseLo = options.ampRanges(1,1);
maxIntenseLo = options.ampRanges(1,2);
minIntenseHi = options.ampRanges(2,1);
maxIntenseHi = options.ampRanges(2,2);

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
list{'Control'}{'trialVarPreToneBias'} = trialVarPreToneBias;
list{'Control'}{'trialVarPreToneLength'} = trialVarPreToneLength;
list{'Control'}{'trialVarLastConsPretone'} = trialVarLastConsPretone;

list{'Control'}{'trialVarStimSeq'} = trialVarStimSeq;