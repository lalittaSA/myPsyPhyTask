function list = AudiDeci_taskCondition_hazard(list,options)

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
%% generate pretone length distribution (100 trials) that minimizes predictable hazard rate (exp decay with cutoff: at 20)
preToneLengthEdges = [1,3,7,13]; % to split preTone lengths into groups
nPreToneGroup = length(preToneLengthEdges)-1;

preToneBiasGroup = [-1 0 1]; % wanna have more balanced trials than all H-L
nPreToneBiasType = length(preToneBiasGroup);

preToneBlockSize = nPreToneGroup*nPreToneBiasType;

x = 1:12; % approximate cutoff (to get the last bin ~1)
expDecay = exp(-x/5.5); % to get mean pretone lenght ~5
%     plot(expDecay); % just for testing
nPreToneDistrib = preToneBlockSize*expDecay/sum(expDecay);
% check if sum = minPreToneBlockSize
nPreToneDistrib = nPreToneDistrib(nPreToneDistrib > 0);
nPreTones = x(1:2:length(nPreToneDistrib))+1;
nPreToneDistrib = nPreToneDistrib(1:2:length(nPreToneDistrib)); % sample only every other pretone lengths
sumDistrib = sum(nPreToneDistrib);
nPreToneDistrib = round(nPreToneDistrib*(preToneBlockSize/sumDistrib));


nPreToneDistrib_all = (nPrior * blockSize * nRep) * nPreToneDistrib;
% group pretone lengths to evenly scatter long pretones in all conditions (as the longer ones are rare)
preToneLengthList = [];
preToneLengthType = [];
for nn = 1:length(nPreTones)
    preToneLengthList = [preToneLengthList;repmat(nPreTones(nn),nPreToneDistrib_all(nn),1)];
    preToneLengthType = [preToneLengthType;repmat(nPreTones(nn),nPreToneDistrib(nn),1)];
end

preToneLengthCounts = histcounts(preToneLengthList,preToneLengthEdges);
preToneLengthNum = discretize(preToneLengthList,preToneLengthEdges);
mid_bins = preToneLengthEdges(1:end-1)'+(preToneLengthEdges(2:end)'-preToneLengthEdges(1:end-1)')/2;
preToneLengthGroups = mid_bins(preToneLengthNum);
preToneGroupLabel = unique([preToneLengthNum,preToneLengthGroups,preToneLengthList],'rows');


%% generate blocks of prior - random order

nBlock = nPrior * nRep * preToneBlockSize;
nTrials = nPrior * blockSize * nRep * preToneBlockSize;
pick_method = 'shuffledEach';
priorConditions = topsConditions();
priorParameter = 'priorLevel';
priorConditions.addParameter(priorParameter, num2cell(priorLevels));
likesPrior = topsFoundation();
priorConditions.addAssignment('priorLevel', likesPrior, '.', 'name');

switch pick_method
    case 'shuffledEach'
        priorConditions.setPickingMethod('shuffledEach',nRep * preToneBlockSize);
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

%% get pretone settings & generate pretone combinations
preToneCombination = options.preToneCombination;

preToneSeq = cell(length(nPreTones),1);
preToneBias = cell(length(nPreTones),1);
preToneSeqNum = cell(length(nPreTones),1);
preToneSelect = cell(length(nPreTones),1);

% preToneSeqAll = [];
preToneBiasAll = [];
% preToneLengthAll = [];

preToneSeqSelect = [];
preToneBiasSelect = [];
preToneGroupSelect = [];
preToneLengthSelect = [];
nLastConsPretone = [];

for pp = 1:length(nPreTones)
    switch preToneCombination
        case 'single',  preToneSeqNum{pp} = repmat(freqTypes',1,nPreTones(pp));
        case 'combine', preToneSeqNum{pp} = combinator(length(freqNames),nPreTones(pp),'c','r');
        case 'permute', preToneSeqNum{pp} = combinator(length(freqNames),nPreTones(pp),'p','r');
    end
    nComb = size(preToneSeqNum{pp},1);
    preToneSeq{pp} = freqNames(preToneSeqNum{pp});
%     preToneSeqAll = [preToneSeqAll;mat2cell([cell2mat(preToneSeq{pp}),repmat('X',nComb,1)],ones(nComb,1),nPreTones(pp)+1)];
%     preToneLengthAll = [preToneLengthAll;repmat(nPreTones(pp),nComb,1)];
    preToneNegPos = ((preToneSeqNum{pp}-1)*2)-1;        % change from 1 & 2 to -1 & 1
    preToneBias{pp} = sum(preToneNegPos,2)/nPreTones(pp);  % ratio L(neg)-H(pos) preTones | range = [-1,1];
    preToneBiasAll = [preToneBiasAll;preToneBias{pp}];
    % preTone bias subselection for each length
    subselect_ind = [ones(nPreToneDistrib_all(pp),1); nComb*ones(nPreToneDistrib_all(pp),1)]; % all L and all H
    tmp_rand = [];
    nBalancePreTone = nPreToneDistrib_all(pp);
    noBiasComp = find(preToneBias{pp} == 0);
    if length(noBiasComp) < nBalancePreTone %excluse all H & all L pretones
        while length(tmp_rand) < nBalancePreTone
            tmp_rand = [tmp_rand; noBiasComp(randperm(length(noBiasComp)))];
        end
        subselect_ind = [subselect_ind; tmp_rand(1:nBalancePreTone)]; % balanced H-L
    else            % if there're more combinations than needed ntrials, pick only ntrials
        tmp_rand = noBiasComp(randperm(length(noBiasComp)));
        subselect_ind = [subselect_ind; tmp_rand(1:nBalancePreTone)]; % balanced H-L
    end
    
    preToneSelect{pp} = subselect_ind;
    preToneBiasSelect = [preToneBiasSelect;preToneBias{pp}(preToneSelect{pp},:)];
    preToneLengthSelect = [preToneLengthSelect;repmat(nPreTones(pp),length(subselect_ind),1)];
    tmp_preTone = mat2cell([cell2mat(preToneSeq{pp}(preToneSelect{pp},:)),repmat('X',length(subselect_ind),1)],ones(length(subselect_ind),1),nPreTones(pp)+1);
    preToneSeqSelect = [preToneSeqSelect;tmp_preTone];
    % look for number of last H/L pretones
    express = 'H+X';
    tmp_H = cellfun(@(x) (nPreTones(pp)+1) - regexpi(x,express),tmp_preTone,'UniformOutput',false);
    ind_H = cell2mat(cellfun(@(x) ~isempty(x),tmp_H,'UniformOutput',false));
    express = 'L+X';
    tmp_L = cellfun(@(x) -((nPreTones(pp)+1) - regexpi(x,express)),tmp_preTone,'UniformOutput',false);
    ind_L = cell2mat(cellfun(@(x) ~isempty(x),tmp_L,'UniformOutput',false));
    tmp_HL = zeros(size(tmp_H));
    tmp_HL(ind_H) = cell2mat(tmp_H);
    tmp_HL(ind_L) = cell2mat(tmp_L);
    nLastConsPretone = [nLastConsPretone;tmp_HL];
    preToneGroupSelect = [preToneGroupSelect;repmat(preToneGroupLabel(pp,1),size(tmp_HL))];
end

% next 2 lines just to check bias type distribution
% preToneBiasEdges = [-1,-0.81,-0.61,-0.41,-0.21,-0.11,0.11,0.21,0.41,0.61,0.81,1];
% preToneBiasBins = histcounts(preToneBiasSelect,preToneBiasEdges);
preToneLengthType = unique(preToneGroupSelect);

%% generate snrLevels & preToneSequence for each priorLevel
trialVarSNR = zeros(size(trialVarPrior));
trialVarPreToneSeq = cell(size(trialVarPrior));
trialVarPreToneBias = zeros(size(trialVarPreToneSeq));
trialVarPreToneLength = zeros(size(trialVarPreToneSeq));
trialVarLastConsPretone = zeros(size(trialVarPreToneSeq));
preToneLengthIndList = 1:length(preToneSeqSelect);

for pp = 1:nPrior
    ind = find(trialVarPrior == priorLevels(pp));
    cur_nT = length(ind);
    min_nT = length(snrValues{pp})*nPreToneGroup*nPreToneBiasType;
    nRepPerPrior = cur_nT/min_nT;
    
    % generate cohLevels
    seqSnrCond = topsConditions();
    seqSnrCond.addParameter('snrLevel', num2cell(snrValues{pp}));
    seqSnrCond.addParameter('preToneBias', num2cell(preToneBiasGroup));
    seqSnrCond.addParameter('preToneLength', num2cell(preToneLengthType));
    
    likesSeqAndSnr = topsFoundation();
    seqSnrCond.addAssignment('snrLevel',likesSeqAndSnr, '.', 'name','{}',{1});
    seqSnrCond.addAssignment('preToneBias',likesSeqAndSnr, '.', 'name','{}',{2});
    seqSnrCond.addAssignment('preToneLength',likesSeqAndSnr, '.', 'name','{}',{3});
    seqSnrCond.setPickingMethod('shuffledEach',nRepPerPrior);
    
    keepGoing = true;
    counter = 1;
    while keepGoing
        seqSnrCond.run();
        trialVarSNR(ind(counter)) = likesSeqAndSnr.name{1};
        tmpBiasInd = likesSeqAndSnr.name{2};
        tmpLegnthGroupInd = likesSeqAndSnr.name{3};
        cur_sel = intersect(find(preToneGroupSelect == tmpLegnthGroupInd & preToneBiasSelect == tmpBiasInd),preToneLengthIndList);
%         while isempty(cur_sel)
%             tmpLegnthGroupInd = tmpLegnthGroupInd-1;
%             cur_sel = intersect(find(preToneGroupSelect == tmpLegnthGroupInd & preToneBiasSelect == tmpBiasInd),preToneLengthIndList);
%         end
        tmp_rand = randperm(length(cur_sel));
        cur_sel = cur_sel(tmp_rand(1));
        trialVarPreToneSeq(ind(counter)) = preToneSeqSelect(cur_sel(1));
        % more information about pretone conditions
        trialVarPreToneBias(ind(counter)) = preToneBiasSelect(cur_sel(1));
        trialVarPreToneLength(ind(counter)) = preToneLengthSelect(cur_sel(1));
        trialVarLastConsPretone(ind(counter)) = nLastConsPretone(cur_sel(1));
        preToneLengthIndList(preToneLengthIndList == cur_sel(1)) = [];
        keepGoing = ~seqSnrCond.isDone;
        counter = counter + 1;
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
list{'Control'}{'trialVarPreToneBias'} = trialVarPreToneBias;
list{'Control'}{'trialVarPreToneLength'} = trialVarPreToneLength;
list{'Control'}{'trialVarLastConsPretone'} = trialVarLastConsPretone;

list{'Control'}{'trialVarStimSeq'} = trialVarStimSeq;

