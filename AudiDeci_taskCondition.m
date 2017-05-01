function list = AudiDeci_taskCondition(list,options)

%% get frequency settings
freqTypes = options.freqTypes;
freqNames = options.freqNames;

%% generate pretone sequence to minimize predictable hazard rate (exp decay with cutoff)
if options.nonUniformPreToneLength
    minPreToneBlockSize = 600;
    x = 1:20; % approximate cutoff (to get the last bin ~1)
    expDecay = exp(-x/5); % I pick 5 just to get the mean ~5 (no better reason than this)
%     plot(expDecay); % just for testing
    nPreToneDistrib = round(minPreToneBlockSize*expDecay/sum(expDecay))
    % check if sum = minPreToneBlockSize
    nPreToneDistrib = nPreToneDistrib(nPreToneDistrib > 0);
    sumDistrib = sum(nPreToneDistrib)
    if sumDistrib > minPreToneBlockSize
        nPreToneDistrib = nPreToneDistrib(1:end-1);
    elseif sumDistrib < minPreToneBlockSize
        disp('check pretone length distribution: trial number is less than minimum block size')
    end
    

    nPreTones = x+1;
    nPreToneDistrib = 3 * nPreToneDistrib; % just to get at least 4 seq types per length (min combination[- -; - +; + -; + +])
%% get pretone settings & generate pretone combinations
else
    nPreTones = options.nPreTones;
end
preToneCombination = options.preToneCombination;

preToneSeq = cell(length(nPreTones),1);
preToneBias = cell(length(nPreTones),1);
preToneSeqNum = cell(length(nPreTones),1);
preToneSelect = cell(length(nPreTones),1);
% preToneBiasEarlyLate = cell(length(nPreTones),1);

preToneSeqAll = [];
preToneBiasAll = [];
preToneLengthAll = [];

preToneSeqSelect = [];
preToneBiasSelect = [];
preToneLengthSelect = [];
% preToneBiasEarlyLateSelect = [];

% subSelect_array = [-1 -1; -1 0; -1 1; -0.5 -0.5; -0.5 0; -0.5 0.5;0 -1; 0 -0.5; 0 0; 0 0.5;0 1;0.5 -0.5;0.5 0;0.5 0.5;1 -1;1 0; 1 1];
% priority_array = [-1 -1;1 1;0 0;0 0];
subSelect_array = [-1; 0; 1];

for pp = 1:2:length(nPreTones)  % sample only every other pretone lengths
    switch preToneCombination
        case 'single',  preToneSeqNum{pp} = repmat(freqTypes',1,nPreTones(pp));
        case 'combine', preToneSeqNum{pp} = combinator(length(freqNames),nPreTones(pp),'c','r');
        case 'permute', preToneSeqNum{pp} = combinator(length(freqNames),nPreTones(pp),'p','r');
    end
    nComb = size(preToneSeqNum{pp},1);
    preToneSeq{pp} = freqNames(preToneSeqNum{pp});
    preToneSeqAll = [preToneSeqAll;mat2cell([cell2mat(preToneSeq{pp}),repmat('X',nComb,1)],ones(nComb,1),nPreTones(pp)+1)];
    preToneLengthAll = [preToneLengthAll;repmat(nPreTones(pp),nComb,1)];
    preToneNegPos = ((preToneSeqNum{pp}-1)*2)-1;        % change from 1 & 2 to -1 & 1
    preToneBias{pp} = sum(preToneNegPos,2)/nPreTones(pp);  % ratio L(neg)-H(pos) preTones | range = [-1,1];
    preToneBiasAll = [preToneBiasAll;preToneBias{pp}];
    % preTone bias subselection for each length
    subselect_ind = [];
    tmp_rand = [];
    if nComb < nPreToneDistrib(pp)
        while length(tmp_rand) < nPreToneDistrib(pp)
            tmp_rand = [tmp_rand randperm(nComb)];
        end
        subselect_ind = tmp_rand(1:nPreToneDistrib(pp));
    else
        subselect_ind = [1 nComb];
        for ss = 1:size(priority_array,1) % usually combinations > ntrials - pick only prioritized combinations
            cur_find = find(all(preToneBiasEarlyLate{pp} == priority_array(ss,:),2));
            if ~(isempty(cur_find))
                if length(cur_find) > 1
                    tmp_rand = randperm(length(cur_find));
                    subselect_ind = [subselect_ind; cur_find(tmp_rand(1))];
                else
                    subselect_ind = [subselect_ind; cur_find];
                end
            end
        end
    end
    
    % preTone subselection of each length (early-late bias:[-1 -0.5 0 0.5 1;-1 -0.5 0 0.5 1] - 25 combinations)
%     if ~mod(nPreTones(pp),2)     % if preToneLength is even, split pretone sequence into early-late
%         halfLength = nPreTones(pp)/2;
%         preToneBiasEarlyLate{pp} = [sum(preToneNegPos(:,1:halfLength),2)/halfLength  sum(preToneNegPos(:,halfLength+1:end),2)/halfLength];
%         subselect_ind = [];
%         if pp < 7
%             for ss = 1:size(subSelect_array,1) % typically combinations < ntrials - take everything
%                 cur_find = find(all(preToneBiasEarlyLate{pp} == subSelect_array(ss,:),2));
%                 if ~(isempty(cur_find))
%                     subselect_ind = [subselect_ind; cur_find];
%                 end
%             end
%         else
%             for ss = 1:size(priority_array,1) % usually combinations > ntrials - pick only prioritized combinations
%                 cur_find = find(all(preToneBiasEarlyLate{pp} == priority_array(ss,:),2));
%                 if ~(isempty(cur_find))
%                     if length(cur_find) > 1
%                         tmp_rand = randperm(length(cur_find));
%                         subselect_ind = [subselect_ind; cur_find(tmp_rand(1))];
%                     else
%                         subselect_ind = [subselect_ind; cur_find];
%                     end
%                 end
%             end
%         end
        
        if options.nonUniformPreToneLength
            tmp_rand = [];
            if length(subselect_ind) > nPreToneDistrib(pp) % if there're more than needed ntrials, pick only ntrials
                tmp_rand = randperm(length(subselect_ind));
                subselect_ind = subselect_ind(tmp_rand(1:nPreToneDistrib(pp)));
            elseif length(subselect_ind) < nPreToneDistrib(pp)
                while length(tmp_rand) < nPreToneDistrib(pp)
                    tmp_rand = [tmp_rand randperm(length(subselect_ind))];
                end
                subselect_ind = subselect_ind(tmp_rand(1:nPreToneDistrib(pp)));
            end
        end
        preToneSelect{pp} = sort(subselect_ind);
        preToneBiasSelect = [preToneBiasSelect;preToneBias{pp}(preToneSelect{pp},:)];
        preToneLengthSelect = [preToneLengthSelect;repmat(nPreTones(pp),length(subselect_ind),1)];
        preToneSeqSelect = [preToneSeqSelect;mat2cell([cell2mat(preToneSeq{pp}(preToneSelect{pp},:)),repmat('X',length(subselect_ind),1)],ones(length(subselect_ind),1),nPreTones(pp)+1)];
        preToneBiasEarlyLateSelect = [preToneBiasEarlyLateSelect;preToneBiasEarlyLate{pp}(preToneSelect{pp},:)];
    end  
end

% next 2 lines just to check bias type distribution
% preToneBiasEdges = [-1,-0.81,-0.61,-0.41,-0.21,-0.11,0.11,0.21,0.41,0.61,0.81,1];
% preToneBiasBins = histcounts(preToneBiasSelect,preToneBiasEdges);

preToneBiasType = unique(preToneBiasSelect);
nPreToneSeqType = size(preToneSeqSelect,1);

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

