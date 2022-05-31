function [] = writeToFile(tempDir,tempData)


fid = fopen(tempDir,'wt');
for k=1:size(tempData,1);
for j=1:size(tempData,2);
fprintf(fid,'%0.7f',tempData(k,j));
fprintf(fid,' ');
end;
fprintf(fid,'\n');
end;
fclose(fid);
