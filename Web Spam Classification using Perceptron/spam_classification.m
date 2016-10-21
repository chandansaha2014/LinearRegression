
%Question: 2.1
load('spamdata.mat')
%Taking transpose of train_data
train_data_transpose = train_data';
%Taking transpose of y_train (Actual observations)
ytrain_transpose = ytrain';

%calculating mean for each features 
%Question: 2.2
train_data_mean = mean(train_data_transpose);
%repeating the mean values for all features across each row
train_data_mean = repmat(train_data_mean,size(train_data_transpose,1),1);
%calculating standard deviation for each feature (across each column) for actual training data
train_data_std = std(train_data_transpose);
%repeating the standard deviation values for all features across each row
train_data_std = repmat(train_data_std,size(train_data_transpose,1),1);
%Normalising the actual training set by subtracting the mean and dividing by standard deviation
norm_train_data = (train_data_transpose - train_data_mean) ./ train_data_std;
%Calculating std and mean of the normalized training set to cross-check the normalization process
a = std(norm_train_data);
b = mean(norm_train_data);
%Using a different function “bsxfun” to subtract the mean: just an additional command
C= bsxfun (@minus, train_data_transpose, mean(train_data_transpose));


%Question:2.3
%Applying the logistic model to the normalized training data
logistic_model = mnrfit(norm_train_data,ytrain_transpose);


%Applying the learnt logistic model to the normalized training data
prob = mnrval(logistic_model, norm_train_data);
%creating a third column based on the condition: If prob(spam)> prob(not spam) , then 1 else 2
for k= 1:size(prob,1)
if (prob(k,1)> prob(k,2))
    prob(k,3)=1;
else
    prob(k,3)=2;
end
end


tabulate(probabilities(:,3));
%   Value    Count   Percent
%       1     1154     37.65%
%       2     1911     62.35%

%Creating a matrix for predictions based on logistic regression model for spam emails predictions
predicted_class = prob(:,3);
C = confusionmat(ytrain_transpose,predicted_class);
training_accuracy = trace(C) ./sum(C(:));
%Training accuracy is 0.9237


%Normalizing the test data Question: 2.4

test_data_transpose = test_data';
train_data_mean = mean(train_data_transpose);
train_data_std = std(train_data_transpose);
%reshaping the train_data_mean according to size of test data
A = repmat(train_data_mean,size(test_data_transpose,1),1);
B = repmat(train_data_std,size(test_data_transpose,1),1);
norm_test_data = (test_data_transpose - A) ./ B;

%Applying the logistic model to test_data set
prob_test = mnrval(logistic_model, norm_test_data);
for k= 1:size(prob_test,1)
    if (prob_test(k,1)> prob_test(k,2))
        prob_test(k,3)=1;
    else 
        prob_test(k,3)=2;
    end
end
predicted_class_test = prob_test(:,3);
ytest_transpose = ytest';
C_test= confusionmat(ytest_transpose,predicted_class_test);
test_accuracy = trace(C_test) ./sum(C_test(:));
%Test accuracy is 0.9180 and training accuracy is 0.9237


%Question: 2.5 Gaussian Naive-Bayes
%developing a Naïve-Bayes based prediction model
O1= NaiveBayes.fit(norm_train_data,ytrain_transpose);
C1= O1.predict(norm_train_data);
cMAT1 = confusionmat(ytrain_transpose,C1);
train_accuracy_NB = trace(cMAT1) ./sum(cMAT1(:));
%Running the learnt Naïve-Bayes model on normalized test data
C1_test= O1.predict(norm_test_data);
cMAT1_test = confusionmat(ytest_transpose,C1_test);
test_accuracy_NB = trace(cMAT1_test) ./sum(cMAT1_test(:));
%Test accuracy for Naive Bayes is 0.8145 and training accuracy is 0.8241


%Question: 2.6 Support Vector Machines

smoopt=svmsmoset('Maxiter',50000);
 
%developing the SVM model on normalized training data
svm=svmtrain(norm_train_data,ytrain_transpose,'autoscale','false','Method','SMO','SMO_Opts',smoopt);

svm_train_prediction= svmclassify(svm,norm_train_data);
svm_confusion_matrix = confusionmat(ytrain_transpose,svm_train_prediction);
train_accuracy_svm = trace(svm_confusion_matrix) ./sum(svm_confusion_matrix(:));
% Training Accuracy - 0.9347
%Running the learnt svm model on normalized test data
svm_test_prediction= svmclassify(svm,norm_test_data);
svm_confusion_matrix_test = confusionmat(ytest_transpose,svm_test_prediction);
test_accuracy_svm = trace(svm_confusion_matrix_test) ./sum(svm_confusion_matrix_test(:));
%Test Accuracy - 0.9303

% Question :2.7 

fprintf('SVM Classifier performs best out of these three methods .Logistic model also has good accuracy for the data set  \n');
fprintf('It is bette to slip some of SPAM messages than letting false negative as in this case important mails would be wrongly classified \n');

