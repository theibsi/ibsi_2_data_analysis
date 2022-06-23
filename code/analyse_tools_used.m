toolsFile = '../data/Tools.csv';
T = readtable(toolsFile);
figure('Position',[-1501 697 979 410]);
subplot(1,3,1)
G = groupcounts(T,'language');
pie(G.GroupCount,G.language)
subplot(1,3,3)
bar(G.GroupCount);
ax = gca;
ax.XTickLabel = G.language;
ylabel('Frequency');


% word cloud example
subplot(1,3,3)
wordcloud(G,'language','GroupCount');

savName = fullfile(['tools_tmp' '.png']);
print('-dpng', savName,'-r500')