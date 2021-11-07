% The Pvalue is for the null hypothesis of the residuals being normally
% distribution. Thus, values less than 0.05 would indicate the residuals
% are statistically significantly different from normal at the 5 % level.
% The Flag that is returned indicates the wrong skewness, if Flag = 1 then
% the residuals have the wrong skewness.

function [teststat, teststatt, Pvalue, Flag] = SqB1Test(eps,n)
    Flag = 0;
    % Calculate the first and second moments for each observation
    M2 = eps.^2;
    M3 = eps.^3;
    
    %calculate the average moment
    mM2 = sum(M2)/(n-1);
    mM3 = sum(M3)/(n-1);
    
    if(0<mM3)
        mM3 = -0.0001;
        Flag = 1;
    end
    teststat = mM3 / (mM2.^(3/2));
    
    m = 1000;
    
    % Calculates the number of time a test statistic calculate from random 
    % draws from a normal distribution is more negative the the one
    % calculated from the data set
    Flag1 = 0;
    for j = 1:m
        for i = 1:n
            S(i) = normrnd(0, sqrt(mM2));
        end
        M2t = S.^2;
        M3t = S.^3;
        mM2t = sum(M2t / (n-1));
        mM3t = sum(M3t / (n-1));
        teststatt(j) = mM3t / (mM2t).^(3/2);
        if(teststatt(j) < teststat) 
            Flag1 = Flag1 + 1;
        end
    end
        
    Pvalue = Flag1/m;   
end
