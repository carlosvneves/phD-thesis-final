% The first version of this code was created by Bodhisattva Sen at Columbia
% University.  What is presented here is a slightly adapted version to aid
% in the estimation of CNLS
%
% Adaptations done by Andrew L. Johnson ajohnson@tamu.edu
%
% Function to compute the CONVEX Regression of the response y on the data matrix x
% Function NEEDS the PACKAGE cvx (see http://cvxr.com/cvx/) to be installed in the computer 

% y: the response variable -- a column vector of length n
% x: the data matrix of the predictors (of size n*p);
% Note that the vector of ones is NOT required in the data matrix

% th: the value of the fitted response

% The function also gives estimates of the sub-gradients at each data point
% and is stored in the 'beta1' matrix

% Note that for sample sizes of 200 or more, the function can be very slow!
% It involves n(n-1) many constraints  

function [eps,alpha1,beta1,yhat] = ComputeConcaveFn(x,y)
n = size(y,1);
% size returns a vector of length 2 with height and width of x
d = size(x,2); 
% Reassign d with the value of d(2) makes d a scalar with the value indicating the 
% number of input variables98+6
l = zeros(n,d);
tic

cvx_begin quiet
    %cvx_precision low
    variable yhat(n)
    variable beta1(n,d)
    minimize(norm(y-yhat))
    subject to
% The purpose of this line is to impose monotonicity by restricting beta1 to be non-negative    
        l <= beta1
% These two loops construct the Afriat inequalities
        for i = 1:n,
            for j = 1:n,
                if(i ~=j) yhat(i) - yhat(j) >= beta1(i,:)*(x(i,:) - x(j,:)).'; end
            end
        end
% Uncommenting this code will lead to a CRS estimator        
%        for i = 1:n,
%            0 == x(i,:)*beta1(:,i) - yhat(i);
%        end
cvx_end

eps = y - yhat;

% Get alpha values
for i = 1:n,
    alpha1(i) = y(i) - eps(i) - beta1(i,:)*(x(i,:)).';
end
alpha1 = alpha1';

toc
end


