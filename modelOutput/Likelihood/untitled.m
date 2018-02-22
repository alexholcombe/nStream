% Add directories
usePath = '~/gitCode/nStream/';
dataDirectory = [usePath 'modelOutput/compiled/'];



% Task parameters
%sampleNames = {'SONA/twoStreams','SONA/eightStreams', 'Pilots/End6Strm82msSOA', 'Pilots/Ex6Strm82msSOA'}; %'Ex8Streams82msSOA', 'Ex6Streams115msSOA'
sampleNames = {};

crowding = {'crowded','bouma'};
rings = {'one', 'two','three'};

for cond = crowding
    for ring = rings
        thisGroup = strcat(cond, '/', ring);
        sampleNames{end+1} = thisGroup{1};
    end
end


for thisSample = 1:6
    splitName = strsplit(sampleNames{thisSample},'/');
    folder = splitName{1}
    group = splitName{2}
    
    % Load data
    cd(dataDirectory);
    load([folder '/CompiledData_TGRSVP_Exp2_' group '.mat']);
    
    for thisParticipant = 1
        for condition = 1
            for stream = 1
                theseErrorsByParticipant = squeeze(compiledErrors(thisParticipant,:,:));
                theseErrorsByParticipant(1:5)
            end
        end
    end
end