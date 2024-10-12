%%  创建cell数组保存数据
data_low_load = cell(30, 9); %   第一列TrailNum，存放试次序号
                             %   第二列SubName，存放被试性别
                             %   第二列SubSex，存放被试性别
                             %   第四列SubAge，存放被试年龄
                             %   第五列GraOrientation，记录测试光栅朝向0为竖直，1为水平
                             %   第六列RespOrientation，记录被试反应0为反应错误，1为反应正确
                             %   第七列TextColor，记录字母颜色0为绿色，1为红色
                             %   第八列RespLowLoad,记录低负荷任务判断，0为错误，1为正确
                             %   第九列RT，反应时
data_high_load = cell(30, 9);   %   第一列TrailNum，存放试次序号
                                %   第二列SubName，存放被试性别
                                %   第二列SubSex，存放被试性别
                                %   第四列SubAge，存放被试年龄
                                %   第五列GraOrientation，记录测试光栅朝向0为竖直，1为水平
                                %   第六列RespOrientation，记录被试反应0为反应错误，1为反应正确
                                %   第七列AlphabetType，记录字母集类型，必含X为0，必含N为1，都不含为2
                                %   第八列RespHighLoad,记录高负荷任务判断，0为错误，1为正确
                                %   第九列RT，反应时
prompt = {'SubName','SubSex[m, f]','SubAge'};
title = 'Exp Info'; 
definput = {'','','',''};           
data_low_load(1:30, 2:4) = repmat(inputdlg(prompt,title,[1, 50],definput)', 30, 1);
data_high_load(1:30, 2:4) = data_low_load(1:30, 2:4);
x_grating = [zeros(1, 15), ones(1, 15)];
x_grating = x_grating(randperm(length(x_grating)));      
x_color = [zeros(1, 15), ones(1, 15)];
x_color = x_color(randperm(length(x_color))); 
x_alphabet = [zeros(1, 10), ones(1, 10), ones(1, 10)*2];
x_alphabet = x_alphabet(randperm(length(x_alphabet))); 
for i = 1:30   
    data_high_load{i, 5} = x_grating(i);
    
    data_high_load{i, 7} = x_alphabet(i);
    
    data_low_load{i, 5} = x_grating(i);
    data_low_load{i, 7} = x_color(i);
end
for ii = 1:30
    data_high_load{ii, 1} = ii;
    data_low_load{ii, 1} = ii;
end
                            
%%  设置按键信息
KbName('UnifyKeyNames');
key.escape = KbName('escape');
key.f = KbName('f');
key.j = KbName('j');
key.n = KbName('n');
key.x = KbName('x');
key.space = KbName('space');


%% 指导与结束语
guide_text.start_high = '欢迎你参加实验2第一部分\n\n接下来您将看到一串随机字符，字符中含X请按X，含N请按N，都不含请按空格。\n\n在呈现字符的同时会呈现光栅1。实验过程先进行字符任务的判断，后出现光栅2，光栅2的朝向始终为水平，\n\n请判断光栅1与2朝向是否一致，如认为一致请按X，认为不一致请按N。 按任意键开始！';
guide_text.start_low = '欢迎您参加实验2第二部分\n\n接下来您将看到一串随机字符，字符为绿色请按X，字符为红色请按N。\n\n在呈现字符同时会呈现光栅1。实验过程先进行字符任务的判断，后出现光栅2，光栅2的朝向始终为水平，\n\n请判断光栅1与2朝向是否一致，如认为一致请按X，认为不一致请按N。按任意键开始！';
guide_text.lxend = '练习结束，开始正式实验!\n\n按任意键继续！';
guide_text.end_high = '实验第一部分结束！\n\n稍作休息，请按任意键继续';
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
mask_dur = 300;                    %视觉掩蔽的时间  300ms
grating_dur_1 = 1000;
grating_dur_2 = 300;
simple_load_dur = 1000;


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
grating_contrast_1 = 0.1;
grating_contrast_test=0.1;
offset = 300;
%%  不同字母集
num_letters = 5;
letter_set.all = ['A':'Z']; 
letter_set.N = ['A':'M', 'O':'W', 'Y', 'Z','N'];
letter_set.X  = ['A':'M', 'O':'W', 'Y', 'Z', 'X'];
letter_set.noXN = setdiff('A':'Z', ['X', 'N']); 


%%   刺激呈现
%%% 呈现开始提示语
Screen('TextSize',windowPtr,40);
Screen('TextFont',windowPtr,'Simsun');
DrawFormattedText(windowPtr,double(guide_text.start_high),'center','center',[0]);
Screen('Flip', windowPtr); 
KbWait;

%%  高负荷任务练习
for trials = 1:5
    grating_contrast_1 = grating_contrast_test;
    grating_angle = angle(randi(2));
    
    % 生成随机字母序列
    letter_sequence.N = [letter_set.N(randi(length(letter_set.N)-1, 1, num_letters-1)), 'N'];
    letter_sequence.X = [letter_set.X(randi(length(letter_set.X)-1, 1, num_letters-1)), 'X'];
    letter_sequence.noXN = letter_set.noXN(randi(length(letter_set.noXN), 1, num_letters));

    letter_sequence_type = randi([0, 2], 1, 1);
    %   高负荷任务字符集选取
    if  letter_sequence_type == 0
        show_letter = letter_sequence.X;
    elseif letter_sequence_type == 1
        show_letter = letter_sequence.N;
    else
        show_letter = letter_sequence.noXN;    
    end
    
    
    grating_texture_1=  make_texture(grating_contrast_1, grating_size, grating_angle, 50, background, windowPtr);
    grating_texture_2=  make_texture(0.5, grating_size, 90, 50, background, windowPtr);
    
    
    for fixation = 1:(fixation_dur/1000)*hz
        CrossFixaPoitRect1=CenterRect([0 0 40 12],windowRect);
        CrossFixaPoitRect2=CenterRect([0 0 12 40],windowRect);
        PoitRect = CenterRect([0, 0, 10, 10], windowRect);
        Screen('FillRect', windowPtr, [0,0,0], CrossFixaPoitRect1);
        Screen('FillRect', windowPtr, [0,0,0], CrossFixaPoitRect2);
        Screen('flip',windowPtr);%呈现
    end


    %%% 呈现视觉掩蔽
    for mask_1 = 1:(mask_dur/1000)*hz
          Screen('DrawTexture',windowPtr,mask_texture,[],mask_pos);%画纹理
          Screen(windowPtr,'Flip');%呈现刺激
    end

    %%% 呈现光栅1和高难度任务
    for grating_1 = 1:(grating_dur_1/1000)*hz
        Screen('DrawTexture',windowPtr,grating_texture_1,[],grating_pos);%画纹理
        DrawFormattedText(windowPtr,show_letter,'center','center',[0 0 0]);
        Screen(windowPtr,'Flip');%呈现光栅
    end

    %%% 呈现视觉掩蔽
    for mask_2 = 1:(mask_dur/1000)*hz
          Screen('DrawTexture',windowPtr,mask_texture,[],mask_pos);%画纹理
          Screen(windowPtr,'Flip');%呈现刺激
    end
    %%% 进行高难度任务的判断
    %%% 按键提示
    Screen(windowPtr,'Flip');
    Screen('TextSize',windowPtr,40);
    Screen('TextFont',windowPtr,'Simsun');
    DrawFormattedText(windowPtr,double(guide_text.judgment),'center','center',[0, 0, 0]);
    Screen('Flip',windowPtr);
    KbWait;
    while true 
        [touch,secs,KbCode] =KbCheck;
        if  KbCode(key.x)
           resp = 0;
           break;
        elseif KbCode(key.n)
           resp = 1;
           break;
        elseif KbCode(key.space)
            resp = 2;
            break;
        elseif  KbCode(key.escape)
           Screen('CloseAll');
           break;
        end
    end
    %按键归类
    if (resp == 0 && letter_sequence_type == 0 ) || (resp == 1 && letter_sequence_type == 1) ||(resp == 2 && letter_sequence_type == 2)
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
    
    
    %%% 呈现光栅2
    for grating_2 = 1:(grating_dur_2/1000)*hz
          Screen('DrawTexture',windowPtr,grating_texture_2,[],grating_pos);%画纹理
          Screen(windowPtr,'Flip');%呈现光栅
    end
    
    Screen(windowPtr,'Flip');
    %%% 判断提示
    Screen('TextSize',windowPtr,40);
    Screen('TextFont',windowPtr,'Simsun');
    DrawFormattedText(windowPtr,double(guide_text.judgment),'center','center',[0, 0, 0]);
    Screen('Flip',windowPtr);
    KbWait;
    
    %%% 进行光栅的判断
    while true            
        [touch,secs,KbCode] =KbCheck;
        if  KbCode(key.x)
            resp = 1;
            break;
        elseif KbCode(key.n)
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

%%  高负荷任务正式实验
for trials = 1:30
    grating_angle = angle(data_high_load{trials, 5}+1);
    % 生成随机字母序列
    letter_sequence.N = [letter_set.N(randi(length(letter_set.N)-1, 1, num_letters-1)), 'N'];
    letter_sequence.X = [letter_set.X(randi(length(letter_set.X)-1, 1, num_letters-1)), 'X'];
    letter_sequence.noXN = letter_set.noXN(randi(length(letter_set.noXN), 1, num_letters));
    %   高负荷任务字符集选取
    if  data_high_load{trials, 7} == 0
        show_letter = letter_sequence.X;
    elseif data_high_load{trials, 7} == 1
        show_letter = letter_sequence.N;
    else
        show_letter = letter_sequence.noXN;    
    end
    
    
    grating_texture_1=  make_texture(0.1, grating_size, grating_angle, 50, background, windowPtr);
    grating_texture_2=  make_texture(0.5, grating_size, 90, 50, background, windowPtr);
    
    
    for fixation = 1:(fixation_dur/1000)*hz
        CrossFixaPoitRect1=CenterRect([0 0 40 12],windowRect);
        CrossFixaPoitRect2=CenterRect([0 0 12 40],windowRect);
        PoitRect = CenterRect([0, 0, 10, 10], windowRect);
        Screen('FillRect', windowPtr, [0,0,0], CrossFixaPoitRect1);
        Screen('FillRect', windowPtr, [0,0,0], CrossFixaPoitRect2);
        Screen('flip',windowPtr);%呈现
    end
    

    %%% 呈现视觉掩蔽
    for mask_1 = 1:(mask_dur/1000)*hz
          Screen('DrawTexture',windowPtr,mask_texture,[],mask_pos);%画纹理
          Screen(windowPtr,'Flip');%呈现刺激
    end 

    %%% 呈现光栅1和高难度任务
    for grating_1 = 1:(grating_dur_1/1000)*hz
        Screen('DrawTexture',windowPtr,grating_texture_1,[],grating_pos);%画纹理
        DrawFormattedText(windowPtr,show_letter,'center','center',[0 0 0]);
        Screen(windowPtr,'Flip');%呈现光栅
    end

    %%% 呈现视觉掩蔽
    for mask_2 = 1:(mask_dur/1000)*hz
          Screen('DrawTexture',windowPtr,mask_texture,[],mask_pos);%画纹理
          Screen(windowPtr,'Flip');%呈现刺激
    end
    
    %%%  进行高难度任务的判断
    Screen(windowPtr,'Flip');%呈现刺激
    
    while true 
        [touch,secs,KbCode] =KbCheck;
        if  KbCode(key.x)
           resp = 0;
           
           break;
        elseif KbCode(key.n)
           resp = 1;
           
           break;
        elseif KbCode(key.space)
            resp = 2;
            
            break;
        elseif  KbCode(key.escape)
            Screen('CloseAll');
            break;
        end
    end
    %按键归类
    if (resp == 0 && data_high_load{trials, 7} == 0 ) || (resp == 1 && data_high_load{trials, 7} == 1) ||(resp == 2 && data_high_load{trials, 7} == 2)
        data_high_load{trials, 8} = 1;
    else
        data_high_load{trials, 8} = 0;
    end
    KbWait;
    %%% 呈现光栅2
    for grating_2 = 1:(grating_dur_2/1000)*hz
          Screen('DrawTexture',windowPtr,grating_texture_2,[],grating_pos);%画纹理
          Screen(windowPtr,'Flip');%呈现光栅
    end
    
    Screen(windowPtr,'Flip');%呈现光栅
    t0 = GetSecs;
    %%% 进行光栅的判断
    while true 
        [touch,secs,KbCode] =KbCheck;
        if  KbCode(key.x)
           resp = 1;
           data_high_load{trials, 9} = GetSecs - t0; % rt 
           break;
        elseif KbCode(key.n)
           resp = 2;
           data_high_load{trials, 9} = GetSecs - t0; % rt 
           break;
        elseif KbCode(key.escape)
           Screen('CloseAll');
           break;
        end
    end
    %按键归类
    if (resp == 1 && grating_angle == 90) || (resp == 2 && grating_angle ~= 90)
        data_high_load{trials, 6} = 1;
    else
        data_high_load{trials, 6} = 0;
    end

end
Screen('Flip',windowPtr);
Screen('TextSize',windowPtr,40);
Screen('TextFont',windowPtr,'Simsun');
DrawFormattedText(windowPtr,double(guide_text.end_high),'center','center',[0]);
Screen('Flip',windowPtr);
WaitSecs(1.5);
KbWait;


Screen('TextSize',windowPtr,40);
Screen('TextFont',windowPtr,'Simsun');
DrawFormattedText(windowPtr,double(guide_text.start_low),'center','center',[0 0 0]);
WaitSecs(1.5);
Screen('Flip', windowPtr); 
KbWait;
%%  低负荷任务练习
for trials = 1:5
    grating_angle = angle(randi(2));
    letter_sequence.all = letter_set.all(randi(length(letter_set.all), 1, num_letters));
    grating_texture_1=  make_texture(grating_contrast_1, grating_size, grating_angle, 50, background, windowPtr);
    grating_texture_2=  make_texture(0.5, grating_size, 90, 50, background, windowPtr);
    color_type = round(rand(1));
    %   低负荷任务刺激颜色选取
    if color_type == 0
        letter_color = [0, 255, 0];
    else
        letter_color = [255 0 0];
    end
        
    
    for fixation = 1:(fixation_dur/1000)*hz
        CrossFixaPoitRect1=CenterRect([0 0 40 12],windowRect);
        CrossFixaPoitRect2=CenterRect([0 0 12 40],windowRect);
        PoitRect = CenterRect([0, 0, 10, 10], windowRect);
        Screen('FillRect', windowPtr, [0,0,0], CrossFixaPoitRect1);
        Screen('FillRect', windowPtr, [0,0,0], CrossFixaPoitRect2);
        Screen('flip',windowPtr);%呈现
    end


    %%% 呈现视觉掩蔽
    for mask_1 = 1:(mask_dur/1000)*hz
          Screen('DrawTexture',windowPtr,mask_texture,[],mask_pos);%画纹理
          Screen(windowPtr,'Flip');%呈现刺激
    end

    %%% 呈现光栅1和低难度任务
    for grating_1 = 1:(grating_dur_1/1000)*hz
        
        Screen('DrawTexture',windowPtr,grating_texture_1,[],grating_pos);%画纹理
        DrawFormattedText(windowPtr,letter_sequence.all,'center','center',letter_color);
        Screen(windowPtr,'Flip');%呈现光栅
    end

    %%% 呈现视觉掩蔽
    for mask_2 = 1:(mask_dur/1000)*hz
          Screen('DrawTexture',windowPtr,mask_texture,[],mask_pos);%画纹理
          Screen(windowPtr,'Flip');%呈现刺激
    end
    
    %%%  进行低难度任务的判断
    Screen(windowPtr,'Flip');%呈现刺激
    
    Screen('TextSize',windowPtr,40);
    Screen('TextFont',windowPtr,'Simsun');
    DrawFormattedText(windowPtr,double(guide_text.judgment),'center','center',[0, 0, 0]);
    Screen('Flip',windowPtr);
    KbWait;
    while true 
        [touch,secs,KbCode] =KbCheck;
        if  KbCode(key.x)
           resp = 0;
           break;
        elseif KbCode(key.n)
           resp = 1;

           break;
        else KbCode(key.escape)
           Screen('CloseAll');
           break;
        
        end
    end
    %按键归类
    if (resp == 0 && color_type == 0) || (resp == 1 && color_type == 1)
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
    
    %%% 呈现光栅2
    for grating_2 = 1:(grating_dur_2/1000)*hz
          Screen('DrawTexture',windowPtr,grating_texture_2,[],grating_pos);%画纹理
          Screen(windowPtr,'Flip');%呈现光栅
    end
    
    %%% 进行光栅的判断
    Screen(windowPtr,'Flip');
    Screen('TextSize',windowPtr,40);
    Screen('TextFont',windowPtr,'Simsun');
    DrawFormattedText(windowPtr,double(guide_text.judgment),'center','center',[0, 0, 0]);
    Screen('Flip',windowPtr);
    KbWait;
    while true 
        [touch,secs,KbCode] =KbCheck;
        if  KbCode(key.x)
            resp = 1;

            break;
        elseif KbCode(key.n)
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

%%  低负荷任务正式实验
for trials = 1:30
    grating_angle = angle(data_low_load{trials, 5}+1);
     %   低负荷任务刺激颜色选取
    if data_low_load{trials, 7} == 0
        letter_color = [0, 255, 0];
    else
        letter_color = [255 0 0];
    end
    grating_texture_1=  make_texture(grating_contrast_1, grating_size, grating_angle, 50, background, windowPtr);
    grating_texture_2=  make_texture(0.5, grating_size, 90, 50, background, windowPtr);
    for fixation = 1:(fixation_dur/1000)*hz
        CrossFixaPoitRect1=CenterRect([0 0 40 12],windowRect);
        CrossFixaPoitRect2=CenterRect([0 0 12 40],windowRect);
        PoitRect = CenterRect([0, 0, 10, 10], windowRect);
        Screen('FillRect', windowPtr, [0,0,0], CrossFixaPoitRect1);
        Screen('FillRect', windowPtr, [0,0,0], CrossFixaPoitRect2);
        Screen('flip',windowPtr);%呈现
    end


    %%% 呈现视觉掩蔽
    for mask_1 = 1:(mask_dur/1000)*hz
          Screen('DrawTexture',windowPtr,mask_texture,[],mask_pos);%画纹理
          Screen(windowPtr,'Flip');%呈现刺激
    end

    %%% 呈现光栅1和低难度任务
    for grating_1 = 1:(grating_dur_1/1000)*hz
        Screen('DrawTexture',windowPtr,grating_texture_1,[],grating_pos);%画纹理
        DrawFormattedText(windowPtr,letter_sequence.noXN,'center','center',letter_color);
        Screen(windowPtr,'Flip');%呈现光栅
    end

    %%% 呈现视觉掩蔽
    for mask_2 = 1:(mask_dur/1000)*hz
          Screen('DrawTexture',windowPtr,mask_texture,[],mask_pos);%画纹理
          Screen(windowPtr,'Flip');%呈现刺激
    end
    %%%  进行低难度任务的判断
    Screen(windowPtr,'Flip');%呈现刺激
    
    while true 
        [touch,secs,KbCode] =KbCheck;
        if  KbCode(key.x)
           resp = 1;
            
           break;
        elseif KbCode(key.n)
           resp = 2;

           break;
        elseif KbCode(key.space)
           Screen('CloseAll');
           break;
        end
    end
    %按键归类
    if (resp == 1 && data_low_load{trials, 7}==0) || (resp == 2 && data_low_load{trials, 7}==1)
        data_low_load{trials, 8} = 1;
    else
        data_low_load{trials, 8} = 0;
    end
    
    %%% 呈现光栅2
    for grating_2 = 1:(grating_dur_2/1000)*hz
          Screen('DrawTexture',windowPtr,grating_texture_2,[],grating_pos);%画纹理
          Screen(windowPtr,'Flip');%呈现光栅
    end
    
    Screen(windowPtr,'Flip');%呈现光栅
    %%% 进行光栅的判断
    t0 = GetSecs;
        while true 
        [touch,secs,KbCode] =KbCheck;
        if  KbCode(key.x)
           resp = 1;
            data_low_load{trials, 9} = GetSecs - t0; % rt
           break;
        elseif KbCode(key.n)
           resp = 2;
            data_low_load{trials, 9} = GetSecs - t0; % rt
           break;
        elseif KbCode(key.escape)
           Screen('CloseAll');
           break;
        end
    end
    %按键归类
    if (resp == 1 && grating_angle == 90) || (resp == 2 && grating_angle~=90)
        data_low_load{trials, 6} = 1;
    else
        data_low_load{trials, 6} = 0;
    end

end
%结束语
Screen('TextSize',windowPtr,40);
Screen('TextFont',windowPtr,'Simsun');
DrawFormattedText(windowPtr,double(guide_text.end),'center','center',[0 0 0]);
Screen('Flip',windowPtr);
WaitSecs(1.5);
Screen('CloseAll');%关闭窗口   

%%  将数据保存为CSV文件
%   低负荷任务数据
header_low_load = {'TrialNum', 'SubName', 'SubSex', 'SubAge','GraOrientation', 'RespOrientation','TextColor'	'RespLowLoad','RT'};
data_table_low_load = cell2table(data_low_load, 'VariableNames', header_low_load);
exp_data_low_load = strcat('data\', 'exp2_2_', char(data_low_load{1,2}), '_', date, '.csv');
writetable(data_table_low_load, exp_data_low_load);

%   高负荷任务数据
header_high_load = {'TrialNum', 'SubName', 'SubSex', 'SubAge','GraOrientation', 'RespOrientation','AlphabetType'	'RespHighLoad','RT'};
data_table_high_load = cell2table(data_high_load, 'VariableNames', header_high_load);
exp_data_high_load = strcat('data\', 'exp2_1_', char(data_high_load{1,2}), '_', date, '.csv');
writetable(data_table_high_load, exp_data_high_load);

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
