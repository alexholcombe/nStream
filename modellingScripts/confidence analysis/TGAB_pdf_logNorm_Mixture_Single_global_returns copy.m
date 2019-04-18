function [result pseudo_normal normFactor_uniform normFactor_normal uniResultTemp normResultTemp normResult uniResult] = TGAB_ExGauss_pdf_Mixture_Single(x,params)
    
    global xDomain;
    global pseudo_uniform;

    params
    
    p = params(1);
    mu = params(2);
    sigma = params(3);

    function [Y] = lognpdfWrapper(X, mu, sigma)
        X = X - mu;
        lognpdf(X, mu, sigma);
    end

%     fprintf('line 8\n')
    pseudo_normal = lognpdfWrapper(xDomain, mu, sigma).*pseudo_uniform;
    
    normFactor_uniform = sum(pseudo_uniform);
    normFactor_normal = sum(pseudo_normal);
    
    if normFactor_uniform == 0
        normFactor_uniform = 10^-8;
    end
    
    if normFactor_normal == 0
        normFactor_normal = 10^-8;
    end
    
%     fprintf('line 22\n')
    uniResultTemp = interp1(xDomain, pseudo_uniform, x);
    normResultTemp = lognpdfWrapper(x, mu, sigma).*uniResultTemp;
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
%     fprintf('tempResult')
%     disp(tempResult)
    
    %xIndex = x-min(xDomain)+1;
    %results = tempResult(xIndex);
    result = tempResult;

end