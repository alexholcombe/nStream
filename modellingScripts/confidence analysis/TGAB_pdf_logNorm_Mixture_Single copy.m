function [result pseudo_normal normFactor_uniform normFactor_normal uniResultTemp normResultTemp normResult uniResult] = TGAB_ExGauss_pdf_Mixture_Single(x,p,mu,sigma)
    
    global xDomain;
    global pseudo_uniform;

    [p mu sigma];

    x = x + exp(mu-sigma^2); %Shift the distribution by the mode
    shiftedXDomain = xDomain + exp(mu-sigma^2);

%     fprintf('line 8\n')
    pseudo_normal = lognpdf(shiftedXDomain, mu, sigma).*pseudo_uniform;
    
    normFactor_uniform = sum(pseudo_uniform);
    normFactor_normal = sum(pseudo_normal);
    
    if normFactor_uniform == 0
        normFactor_uniform = 10^-8;
    end
    
    if normFactor_normal == 0
        normFactor_normal = 10^-8;
    end
    
%     fprintf('line 22\n')
    uniResultTemp = interp1(shiftedXDomain, pseudo_uniform, x);
    normResultTemp = lognpdf(x, mu, sigma).*uniResultTemp;
%     fprintf('normResultTemp')
%     disp(normResultTemp)
    
    uniResultTemp = uniResultTemp/normFactor_uniform;
    normResultTemp = normResultTemp/normFactor_normal;
    
    propNorm = p;
    propUniform = 1-p;
    
    normResult = propNorm*normResultTemp;
    uniResult = propUniform*uniResultTemp;
    
    if sum(size(normResult)==size(uniResult))==2
        tempResult = normResult+uniResult;
    else
        tempResult = normResult+uniResult';
    end


    if any(isnan(tempResult))
    	tempResult(find(isnan(tempResult))) = 0; %replace NaNs with 0
    end
%     fprintf('tempResult')
%     disp(tempResult)
    
    %xIndex = x-min(xDomain)+1;
    %results = tempResult(xIndex);

    if(any(tempResult == Inf))
        x(find(tempResult==Inf))
        uniResultTemp(find(tempResult==Inf))
        normResultTemp(find(tempResult==Inf))
    end


    if(any(tempResult == 0))
        x(find(tempResult==0))
        uniResultTemp(find(tempResult==0))
        normResultTemp(find(tempResult==0))
    end

    result = tempResult;

end