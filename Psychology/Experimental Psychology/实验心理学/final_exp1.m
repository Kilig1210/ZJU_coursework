%%  创建cell数组保存数据

data = cell(90,9);  %   第一列TrailNum，存放试次序号
                    %   第二列SubName，存放被试性别
                    %   第二列SubSex，存放被试性别
                    %   第四列SubAge，存放被试年龄
                    %   第五列GraOrientation，记录测试光栅朝向0为竖直，1为水平
                    %   第六列RespOrientation，记录被试反应0为反应错误，1为反应正确
                    %   第七列GuePos，记录提示位置，-1为提示在左侧，0为中性提示，1为提示在右侧
                    %   第八列GratingPos，记录光栅和musk位置，-1为在左侧，1为在右侧
                    %   第九列RT
prompt = {'SubName','SubSex[m, f]','SubAge'};
title = 'Exp Info'; 
definput = {'','','',''}; 
data(1:90, 2:4) = repmat(inputdlg(prompt,title,[1, 50],definput)', 90, 1);
x = [zeros(1, 45), ones(1, 45)];
x = x(randperm(length(x)));                  
for i = 1:90    
    data{i, 5} = x(i);
end
for ii = 1:90
    data{ii, 1} = ii;
end

temp_1 = randi([0 1], 60, 2) * 2 - 1; % 将0转换为-1，1保持不变
% 确定80%的行需要让第一列和第二列相同
num_same = round(0.8 * size(temp_1, 1)); % 确定需要让第一列和第二列相同的行数
same_rows = randperm(size(temp_1, 1), num_same); % 随机选择需要让第一列和第二列相同的行索引
temp_1(same_rows, 2) = temp_1(same_rows, 1); % 让这些行的第二列等于第一列
% 确定20%的行需要让第一列和第二列不同
num_diff = size(temp_1, 1) - num_same; % 确定需要让第一列和第二列不同的行数
diff_rows = setdiff(1:size(temp_1, 1), same_rows); % 剩下的行需要让第一列和第二列不同
diff_rows = diff_rows(1:num_diff); % 从这些行中随机选择需要让第一列和第二列不同的行索引
temp_1(diff_rows, 2) = -temp_1(diff_rows, 1); % 让这些行的第二列和第一列相反
% 生成30行2列的零矩阵
temp_2 = zeros(30, 2);
% 生成随机排列的15个1和15个-1
values = [ones(15,1); -ones(15,1)];
values = values(randperm(length(values)));
% 将随机排列的15个1和15个-1赋值给第二列
temp_2(:, 2) = values;

temp = [temp_1;temp_2];
temp = temp(randperm(size(temp, 1)),:);

for i=1:90
    for ii=1:2
        data{i,ii+6} = temp(i, ii);
    end
end

%%  设置按键信息
KbName('UnifyKeyNames');
key.escape = KbName('escape');
key.f = KbName('f');
key.j = KbName('j');


%%  设置指导与结束语
guide_text.start = '欢迎你参加实验\n\n请判断光栅1和光栅2朝向是否一致，在光栅2呈现完毕后，如认为一致请按F键，认为不一致请按J键。\n\n其中光栅2朝向始终为水平方向，您只需要记忆光栅1的朝向。\n\n先进行练习，按任意键开始！';
guide_text.lxend = '练习结束，开始正式实验!\n\n按任意键继续！';
guide_text.end = '实验结束！\n\n谢谢您的参与！';
guide_text.judgment = '请判断';


%%  设置窗口
background=128;
Screen('Preference', 'SkipSyncTests', 1);
[windowPtr, windowRect]=Screen('OpenWindow', 0, background);
HideCursor;
[x_center, y_center] = RectCenter(windowRect);
hz=Screen('FrameRate',windowPtr);%获取当前刷新率
frame_dur=1/hz;                   %单位，秒

%%  设置刺激呈现时间
fixation_dur = 300;                %注视点呈现时间  300ms
fixacue_dur = 300;                  %线索提示呈现时间 100ms
mask_dur = 300;                    %视觉掩蔽的时间  300ms
grating_dur_1 = 1000;
grating_dur_2 = 300;


%%  设置mask的信息
mask_contrast=0.45; %对比度
mask_size=200;%刺激大小
noise_pix=5;%每个噪音点的像素数；
noise_num=round(mask_size/noise_pix);%水平或竖直方向上的噪音点个数
mask_size=noise_pix*noise_num;%重新计算刺激大小
mask_pos=CenterRect([0 0 mask_size mask_size],windowRect);%%刺激呈现于屏幕中央,刺激的位置矩阵
[x,y]=meshgrid(round(-mask_size/2):round(mask_size/2)-1,round(-mask_size/2):round(mask_size/2)-1);
mask_mask_radius=mask_size/2;
sd=50;
mask_circle_mask=(x.^2+y.^2 <= mask_mask_radius^2);%生成圆形mask
noise_matrix=rand(noise_num,noise_num)*2-1;
temp0=ones(noise_pix);
mask_matrix0=kron(noise_matrix,temp0);%生成像素噪音点，值范围-1~1; kron命令是扩大每个noise元素的值为设定的噪音像素大小
mask_matrix=background*(1+mask_contrast*mask_matrix0.*mask_circle_mask); %刺激矩阵
mask_texture=Screen('MakeTexture',windowPtr, mask_matrix);%把矩阵变为纹理

%%  光栅参数初始化
angle = [0, 90];
grating_size = 200;
grating_pos=CenterRect([0 0 grating_size grating_size],windowRect);
grating_contrast_1 = 0.005;
grating_contrast_test=0.1;
offset = 500;

%%  刺激呈现
%%% 呈现开始提示语
Screen('TextSize',windowPtr,40);
Screen('TextFont',windowPtr,'Simsun');
DrawFormattedText(windowPtr,double(guide_text.start),'center','center',[0]);
Screen('Flip', windowPtr); 
%while KbCheck end
KbWait;
%%  练习
for trials = 1:10
    grating_contrast_1 = grating_contrast_test;
    grating_angle = angle(randi(2));
    offset_temp = (2*round(rand(1))-1)*offset;
    if  randi([0, 2], 1, 1)==0
        cue_color =[0 0 0];
        cue_pos=[x_center, y_center];
    elseif  randi([0, 2], 1, 1)==1
        cue_color =[255 255 255];
        cue_pos =[x_center+14 y_center];
    else
        cue_color =[255 255 255];
        cue_pos =[x_center-14 y_center];
    end
    
    
    
    grating_texture_1=  make_texture(grating_contrast_1, grating_size, grating_angle, 50, background, windowPtr);
    grating_texture_2=  make_texture(0.5, grating_size, 90, 50, background, windowPtr);
    %%% 呈现注视点
    for fixation = 1:(fixation_dur/1000)*hz
        CrossFixaPoitRect1=CenterRect([0 0 40 12],windowRect);
        CrossFixaPoitRect2=CenterRect([0 0 12 40],windowRect);
        PoitRect = CenterRect([0, 0, 10, 10], windowRect);
        Screen('FillRect', windowPtr, [0,0,0], CrossFixaPoitRect1);
        Screen('FillRect', windowPtr, [0,0,0], CrossFixaPoitRect2);
        Screen('flip',windowPtr);%呈现
    end
    
    %%% 呈现线索提示
    for fixation_cue = 1:(fixacue_dur/1000)*hz

        CrossFixaPoitRect1=CenterRect([0 0 40 12],windowRect);
        CrossFixaPoitRect2=CenterRect([0 0 12 40],windowRect);
        Screen('FillRect', windowPtr,[0 0 0], CrossFixaPoitRect1);
        Screen('FillRect', windowPtr,[0 0 0], CrossFixaPoitRect2);
        Screen('DrawDots', windowPtr,cue_pos , 13, cue_color);
        Screen('flip',windowPtr);%呈现
    end
    
    %%% 呈现视觉掩蔽
    for mask_1 = 1:(mask_dur/1000)*hz
          Screen('DrawTexture',windowPtr,mask_texture,[],mask_pos+[offset_temp 0 offset_temp 0]);%画纹理
          Screen(windowPtr,'Flip');%呈现刺激
    end

    %%% 呈现光栅1
    for grating_1 = 1:(grating_dur_1/1000)*hz
    
          Screen('DrawTexture',windowPtr,grating_texture_1,[],grating_pos+[offset_temp 0 offset_temp 0]);%画纹理
          Screen(windowPtr,'Flip');%呈现光栅
    end

    %%% 呈现视觉掩蔽
    for mask_2 = 1:(mask_dur/1000)*hz
          Screen('DrawTexture',windowPtr,mask_texture,[],mask_pos+[offset_temp 0 offset_temp 0]);%画纹理
          Screen(windowPtr,'Flip');%呈现刺激
    end

    %%% 呈现光栅2
    for grating_2 = 1:(grating_dur_2/1000)*hz
          
          Screen('DrawTexture',windowPtr,grating_texture_2,[],grating_pos+[offset_temp 0 offset_temp 0]);%画纹理
          Screen(windowPtr,'Flip');%呈现光栅
    end
    
    %%% 按键提示
    Screen(windowPtr,'Flip');
    Screen('TextSize',windowPtr,40);
    Screen('TextFont',windowPtr,'Simsun');
    DrawFormattedText(windowPtr,double(guide_text.judgment),'center','center',[0, 0, 0]);
    Screen('Flip',windowPtr);
    KbWait;
    
    while true 
        [touch,secs,KbCode] =KbCheck;
        if  KbCode(key.f)
           resp = 1;

           break;
        elseif KbCode(key.j)
           resp = 2;

           break;
        elseif KbCode(key.escape)
           Screen('CloseAll');
           break;
        end
    end
    %按键归类
    if (resp == 1 && grating_angle == 90) || (resp == 2 && grating_angle ~= 90)
        response = 1;
    else
        response = 0;
    end
    %正误反馈
    if response == 1
        feed = '练习正确';
        feed_color = [0 255 0];
    else 
        feed = '练习错误';
        feed_color = [255 0 0];
    end
    Screen('TextSize',windowPtr,40);
    Screen('TextFont',windowPtr,'Simsun');
    DrawFormattedText(windowPtr,double(feed),'center','center',feed_color);
    Screen('Flip',windowPtr);
    WaitSecs(3);
end

Screen('TextSize',windowPtr,40);
Screen('TextFont',windowPtr,'Simsun');
DrawFormattedText(windowPtr,double(guide_text.lxend),'center','center',[0 0 0]);
Screen('Flip',windowPtr);
KbWait;

%%  正式实验
for trials = 1:90
    grating_angle = data{trials, 5};
    
    if  data{trials, 7}==0
        cue_color =[0 0 0];
        cue_pos=[x_center, y_center];
    elseif  data{trials, 7}==1
        cue_color =[255 255 255];
        cue_pos =[x_center+14 y_center];
    else
        cue_color =[255 255 255];
        cue_pos =[x_center-14 y_center];
    end
    offset_temp = data{trials, 8} * offset;
    grating_texture_1=  make_texture(0.005, grating_size, grating_angle, 50, background, windowPtr);
    grating_texture_2=  make_texture(0.5, grating_size, 90, 50, background, windowPtr);
    for fixation = 1:(fixation_dur/1000)*hz
        CrossFixaPoitRect1=CenterRect([0 0 40 12],windowRect);
        CrossFixaPoitRect2=CenterRect([0 0 12 40],windowRect);
        Screen('FillRect', windowPtr,[0 0 0], CrossFixaPoitRect1);
        Screen('FillRect', windowPtr,[0 0 0], CrossFixaPoitRect2);
        
        Screen('flip',windowPtr);%呈现
    end
    Screen('flip',windowPtr);
    %%% 线索提示
    for fixation_cue = 1:(fixacue_dur/1000)*hz

        CrossFixaPoitRect1=CenterRect([0 0 40 12],windowRect);
        CrossFixaPoitRect2=CenterRect([0 0 12 40],windowRect);
        Screen('FillRect', windowPtr,[0 0 0], CrossFixaPoitRect1);
        Screen('FillRect', windowPtr,[0 0 0], CrossFixaPoitRect2);
        Screen('DrawDots', windowPtr,cue_pos , 13, cue_color);
        Screen('flip',windowPtr);%呈现
    end
    
    %%% 呈现视觉掩蔽
    for mask_1 = 1:(mask_dur/1000)*hz
          Screen('DrawTexture',windowPtr,mask_texture,[],mask_pos+[offset_temp 0 offset_temp 0]);%画纹理
          Screen(windowPtr,'Flip');%呈现刺激
    end

    %%% 呈现光栅1
    for grating_1 = 1:(grating_dur_1/1000)*hz

          Screen('DrawTexture',windowPtr,grating_texture_1,[],grating_pos+[offset_temp 0 offset_temp 0]);%画纹理
          Screen(windowPtr,'Flip');%呈现光栅
    end

    %%% 呈现视觉掩蔽
    for mask_2 = 1:(mask_dur/1000)*hz
          Screen('DrawTexture',windowPtr,mask_texture,[],mask_pos++[offset_temp 0 offset_temp 0]);%画纹理
          Screen(windowPtr,'Flip');%呈现刺激
    end

    %%% 呈现光栅2
    for grating_2 = 1:(grating_dur_2/1000)*hz
          
          Screen('DrawTexture',windowPtr,grating_texture_2,[],grating_pos++[offset_temp 0 offset_temp 0]);%画纹理
          Screen(windowPtr,'Flip');%呈现光栅
    end
    Screen(windowPtr,'Flip');

    t0 = GetSecs;
    while true 
        [touch,secs,KbCode] =KbCheck;
        if  KbCode(key.f)
           resp = 1;
           data{trials, 9} = GetSecs - t0; % rt 
           break;
        elseif KbCode(key.j)
           resp = 2;
           data{trials, 9} = GetSecs - t0; % rt
           break;
        elseif KbCode(key.escape)
           Screen('CloseAll');
           break;
        
                
        end
    end
    %按键归类
    if (resp == 1 && grating_angle == 90) || (resp == 2 && grating_angle ~= 90)
        data{trials, 6} = 1;
    else
        data{trials, 6} = 0;
    end

end

%结束语
Screen('TextSize',windowPtr,40);
Screen('TextFont',windowPtr,'Simsun');
DrawFormattedText(windowPtr,double(guide_text.end),'center','center',[0, 0, 0]);
Screen('Flip',windowPtr);
WaitSecs(1.5);
Screen('CloseAll');%关闭窗口

% 将数据保存为CSV文件
header = {'TrialNum', 'SubName', 'SubSex', 'SubAge','GraOrientation', 'RespOrientation','CuePos', 'GratingPos', 'RT'};
data_table = cell2table(data, 'VariableNames', header);
exp_data = strcat('data\', 'exp1_', char(data{1,2}), '_', date, '.csv');
writetable(data_table, exp_data);

function grating_texture = make_texture(contrast, size, angle, period, background, windowPtr)
    
    [x,y]=meshgrid(-size/2:size/2,-size/2:size/2);
    
    period=50;
    space_fre_1=1/period;
    a1=2*pi*space_fre_1*cos(angle*pi/180);
    b1=2*pi*space_fre_1*sin(angle*pi/180);
    mask_radius_1=size/2;
    circle_mask_1=(x.^2+y.^2 <= mask_radius_1^2);%生成圆形mask
    grating_matrix=background*(1+contrast*sin(a1*x+b1*y).*circle_mask_1);   %生成光栅矩阵
    grating_texture=Screen('MakeTexture',windowPtr, grating_matrix);%把矩阵变为纹理
end