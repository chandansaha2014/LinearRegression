
% Set Directory
%DIRNAME ='../Data/'

% variables
% different fractions of training data out of the total data
TRAIN_FRACS = .1:.1:.9;
%TRAIN_FRACS = .5;

NUM_TRAIN_FRACS = length(TRAIN_FRACS);
RUNS_PER_FRAC = 5;

% error ratio on training set
%trainErrorMat    = zeros(RUNS_PER_FRAC, NUM_TRAIN_FRACS);
trainErrorMat    = zeros(RUNS_PER_FRAC);
trainAcc = zeros(RUNS_PER_FRAC);
% error ratio on test set (when using constant weight vector - the one
% generated after training stage).
testErrorMat_const_w   = zeros(RUNS_PER_FRAC);%, NUM_TRAIN_FRACS);
% error ratio on test set (when continuing updating the weight vector).
testErrorMat_updated_w = zeros(RUNS_PER_FRAC);%, NUM_TRAIN_FRACS);
testAccMat_updated_w = zeros(RUNS_PER_FRAC);
% false positive ratio on test set (when using constant weight vector - the
% one generated after training stage).
testFalsePosMat_const_w  = zeros(RUNS_PER_FRAC);%, NUM_TRAIN_FRACS);
% false positive ratio on test set (when continuing updating the weight vector).
testFalsePosMat_updated_w  = zeros(RUNS_PER_FRAC);%, NUM_TRAIN_FRACS);


% Read Data
data = csvread('Sample_Data_1000.csv');

data = data (1:1000 , 1:end) ;


perm = randperm(size(data, 1)); 


% Split Data into training and testing
data = data(perm,:);
train = data(2:601 , 1:end-1);
test = data(602:1000 , 1:end-1); %need to change later
%delete(data);
for iTrainFrac = 1:NUM_TRAIN_FRACS
    trainFrac = TRAIN_FRACS(iTrainFrac);
    display(trainFrac)
    for run=1:RUNS_PER_FRAC
        display(run);
        %train = importdata(fname);
        %test  = importdata(fname);
        
        % Feature and Label
        
        % the vectors without the labels
        trainVectors = train(:,3:end); %
        % the lables
        trainLabels = train(:,2);
        
        % the vectors without the labels
        testVectors = test(:,3:end); %
        % the lables
        testLabels = test(:,2);
        
        
        % add new column of -1 (by this, we can work with hyperplane which contains
        % the origin (0)
        trainVectors = addDummyColumn(trainVectors);
        testVectors  = addDummyColumn(testVectors);
        
        % initial weight vector
        num_of_features = size(trainVectors, 2);
        initial_w = zeros(1, num_of_features);
        
        % perform the algorithm
        % w is the obtained weight vector
        [trainErrorMat(run,iTrainFrac) , ...
            false_positives_ratio, ...
            w ]...            
            = perceptronAlg(initial_w, trainVectors, trainLabels);
        
%         % check on test set - continue updating the weight vector
        [testErrorMat_updated_w(run,iTrainFrac), ...
            testFalsePosMat_updated_w(run,iTrainFrac), ...
            w ] ...
            = perceptronAlg(w, testVectors, testLabels);
        
    end
  end

meanTrainErrorMat = mean(trainErrorMat, 1);
meanTestErrorMat_updated_w = mean(testErrorMat_updated_w, 1);
meanTestFalsePosMat_updated_w = mean(testFalsePosMat_updated_w, 1);

h = figure; 
hold on;
plot(TRAIN_FRACS,meanTrainErrorMat, 'b-o');
plot(TRAIN_FRACS,meanTestErrorMat_updated_w, 'r-.o');
plot(TRAIN_FRACS,meanTestFalsePosMat_updated_w, 'g-.o');

xlabel('Training Fraction');
ylabel('Accuracy rate');
legend('Train', 'Test (updated w)');
txt = sprintf('Average of %d runs per training size', RUNS_PER_FRAC);
title(txt)
fname = sprintf('results_%s.fig', datestr(now, 'dd.mm.yy_HH.MM.SS'));
saveas(h, fname);

























