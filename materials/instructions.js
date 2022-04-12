/**
 * title: text for instructions in experiment 210510
 * author: fred zenker (https://www.fredzenker.com)
 */

// *** global ***

var inactive_btn = 
    '.inactive-btn {display: inline-block; padding: 6px 12px; margin: 0px; font-size: 14px; font-weight: 400; font-family: "Open Sans", "Arial", sans-serif; cursor: pointer; line-height: 1.4; text-align: center; white-space: nowrap; vertical-align: middle; background-image: none; border: 1px solid transparent; border-radius: 4px; color: #333; background-color: #fff; border-color: #ccc; pointer-events: none;}'
;

var en_start_experiment_label = '<span style="font-size:145%;">Start the experiment</span>';
var ko_start_experiment_label = '<span style="font-size:145%;">실험 시작</span>';
var zh_start_experiment_label = '<span style="font-size:145%;">開始實驗</span>';

var en_start_task_label = '<span style="font-size:125%;">Start the task</span>';
var ko_start_task_label = '<span style="font-size:125%;">과제 시작</span>';
var zh_start_task_label = '<span style="font-size:125%;">開始測驗</span>';

var en_continue_label = 'Continue';
var ko_continue_label = '계속';
var zh_continue_label = '繼續';

var en_submit_label = 'Submit';
var ko_submit_label = '<span style="font-size:125%;">제출</span>';
var ko_submit2_label = '제출';
var zh_submit_label = '提交';

var en_progress = 'Completion Progress';
var ko_progress = '진행 상황';
var zh_progress = '完成進度';

var en_incorrect_stimulus =
    '<span style="color:red;font-size:30px;">Incorrect. Please read more carefully.</span>'
;
var ko_incorrect_stimulus =
    '<span style="color:red;font-size:30px;">오답입니다. 더욱 주의깊게 읽어주시기 바랍니다.</span>'
;
var zh_incorrect_stimulus =
    '<span style="color:red;font-size:30px;">不正確。 請仔細閱讀。</span>'
;

var en_rushing_stimulus =
    '<span style="color:red;font-size:30px;">You\'re going too fast. Please read the sentences more carefully.</span>'
;
var ko_rushing_stimulus =
    '<span style="color:red;font-size:30px;">속도가 지나치게 빠릅니다. 문장을 더욱 주의 깊게 읽어주시기 바랍니다.</span>'
;
var zh_rushing_stimulus =
    '<span style="color:red;font-size:30px;">您回答得太快了。 請更仔細地閱讀句子。</span>'
;

var repeat_instructions_header =
    '<img src="img_warning.png" height="150" title="image credit: Flaticon / Freepik (www.freepik.com)"><br>'
;

var en_repeat_instructions_text =   
    '<p>You did not make enough correct responses to continue.</p>'+
    '<p>Now you will have a chance to repeat the instructions and practice trials.</p>'
;
var ko_repeat_instructions_text = 
    '<p>오답이 많아 다음 단계로 넘어갈 수 없습니다.</p>'+
    '<p>과제 설명을 다시 읽고 연습 문제에 다시 답변하여 주시기 바랍니다.</p>'
;
var zh_repeat_instructions_text =   
    '<p>您答對的題目不夠多，因此無法繼續實驗。</p>'+
    '<p>現在您將有機會重複閱讀指示和練習題目。</p>'
;

var en_repeat_instructions_stimulus =
    repeat_instructions_header+en_repeat_instructions_text
;
var ko_repeat_instructions_stimulus =
    repeat_instructions_header+ko_repeat_instructions_text
;
var zh_repeat_instructions_stimulus =
    repeat_instructions_header+zh_repeat_instructions_text
;

// *** preload media ***

var media = [
    'img_login.jpg',
    'img_overview.jpg',
    'img_consent.jpg',
    'img_warning.png',
    'img_checked.png',
    'img_cancel.png',
    'img_loading.jpg',
    'img_man1.png',
    'img_man2.png',
    'img_man3.png',
    'img_man4.png',
    'img_woman1.png',
    'img_woman2.png',
    'img_woman3.png',
    'img_woman4.png',
    'img_overview.jpg',
    'img_recorder.png',
    'img_lbq.jpg',
    'img_ept.jpg',
    'img_practice.jpg',
    'img_success.jpg',
    'img_spr.jpg',
    'img_segmentation.gif',
    'img_question.gif',
    'img_question-korean.gif',
    'img_question-mandarin.gif',
    'img_ajt.jpg',
    'img_ctest.jpg',
    'img_exit.jpg',
    'img_payment.jpg',
    'img_confirmation.jpg'
];

// *** login page ***

// task identifiers

var session_id = 'apple|basil|coral|dream|ember';

// login page html

var login_header = 
    '<img src="img_login.jpg" height="150" title="image credit: vectorjuice / Freepik (www.freepik.com)"><br>'
;

var en_login_text = 
    '<span style="font-size:125%;"><b>Participant ID</b></span><p>Please enter the participant ID assigned to you by the researcher</p><p><input name="pid" type="text" required></p>'
;
var en_login_text_v2 = 
    '<span style="font-size:125%;"><b>Login</b></span>'+
    '<p>Please enter the participant ID and task ID assigned to you by the researcher.</p>'+
    '<p>Participant ID: <input name="pid" type="text" required></p>'+
    '<p>Task ID: <input name="sid" type="text" pattern="'+session_id+'" required></p>'+
    '<p style="width:70%; margin:auto; text-align:left;">At the end of each task, you will receive a new task id that will allow you to proceed to the next task. You can do all the tasks at once or spread them out over multiple sessions as long as you complete them all within seven days.</p><br>'
;
var ko_login_text = 
    '<span style="font-size:125%;"><b>로그인</b></span>'+
    '<p>연구자에게 배정받은 참가ID와 과제ID를 입력하여 주십시오.</p>'+
    '<p>참가ID: <input name="pid" type="text" required></p>'+
    '<p>과제ID: <input name="sid" type="text" pattern="'+session_id+'" required></p>'+
    '<p style="width:70%; margin:auto; text-align:left;">각 과제의 마지막 부분에서 다음 과제로 진행할 수 있는 새로운 과제ID가 주어질 것입니다. 모든 과제를 연속으로 한 번에 하거나, 또는 7일 이내에 실험을 완료하기만 한다면 과제 별로 나눠서 하실 수 있습니다.</p><br>'
;
var zh_login_text = 
    '<span style="font-size:125%;"><b>登入</b></span>'+
    '<p>請輸入研究人員給您的受試者序號以及測驗序號。</p>'+
    '<table style="margin:auto;margin-bottom: 1em;">'+
        '<tr>'+
            '<td>'+
                '受試者序號：'+
            '</td>'+
            '<td>'+
                '<input name="pid" type="text" required>'+
            '</td>'+
        '</tr>'+
        '<tr>'+
            '<td>'+
                '測驗序號：'+
            '</td>'+
            '<td>'+
                '<input name="sid" type="text" pattern="'+session_id+'" required>'+
            '</td>'+
        '</tr>'+
    '</table>'+
    '<p style="width:70%; margin:auto; text-align:left;">在每個測驗的最後，您會收到一組新的測驗序號。透過這組序號，您可以繼續下一個測驗。您可以選擇一次做完所有測驗，或者在七天內分批做完各項測驗。整個實驗必須使用能連接穩定網路的筆電或桌電，而且要能開啟Google或Firefox瀏覽器。請務必不要使用手機或平板來完成實驗。請確認您使用的設備可以錄音，因為其中一項測驗將會需要您錄製簡短的音檔。大多數的電腦都有內建的麥克風可以錄音。另外，請確認您的瀏覽器可以顯示跳出式視窗。</p><br>'
;

var en_login_html =
    login_header+en_login_text
;
var ko_login_html =
    login_header+ko_login_text
;
var zh_login_html =
    login_header+zh_login_text
;

// *** overview ***

var overview_header =
    '<style>fieldset {border:1px solid #999;box-shadow:2px 2px 5px #999;} legend {background:#fff;text-align:left;font-size:110%;} p {text-align:left;} ul {text-align:left}</style>'+ 
    '<img src="img_overview.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)">'
;

var en_overview_text =
    '<h3 style="text-align:center">Reading Sentences and Answering Questions in English</h3>'+
    '<p>Welcome! In this experiment, you will complete a language background survey and four language tasks that involve reading, rating, and recording sentences in English. The entire experiment should take about 45&ndash;60 minutes.</p>'+
    '<fieldset><legend><b>Eligibility</b></legend>'+
    '<ul>'+
    '<li>18 years of age or older</li>'+
    '<li>Native speaker of English</li>'+
    '<li>No advanced courses on English syntax</li>'+
    '</ul>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>Equipment Needed</b></legend>'+
    '<p>The experiment must be completed on a laptop or desktop computer running the Chrome or Firefox browser. Do not attempt to do it on a smartphone or a tablet. Because one of the tasks involves recording short audio samples, you should make sure that your computer is capable of recording audio before starting the experiment. Most computers have built-in microphones that allow users to make audio recordings.</p>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>Eliminating Distractions</b></legend>'+
    '<p>This experiment collects valuable data for a dissertation research project. It is therefore important that you give the tasks your full attention and that you make sure you are in a quiet and distraction-free location before getting started. Please close all unrelated browser windows and refrain from using resources such as dictionaries and search engines while you are participating.</p>'+
    '</fieldset><br>'
;
var en_overview_text_v2 = 
    '<h3 style="text-align:center">Reading Sentences and Answering Questions in English</h3>'+
    '<p>Welcome! In this experiment, you will complete a language background survey and four language tasks that involve reading, rating, and recording sentences in English. </p>'+
    '<fieldset><legend><b>Overview</b></legend>'+
    '<p>This experiment consists of five tasks designed to test how people process certain types of sentences in their first language and/or a second language. You can do all the tasks at once or spread them out over multiple days as long as you complete the entire experiment within seven days. At the end of each task, you will receive a new task ID that will allow you to proceed to the next task. The entire experiment should take 60-90 minutes.</p>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>Eligibility Criteria</b></legend>'+
    '<ul>'+
    '<li>You are 18 years of age or older</li>'+
    '<li>You are a native speaker of Mandarin Chinese</li>'+
    '<li>You started learning English at age 8 or older</li>'+
    '</ul>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>Required Equipment</b></legend>'+
    '<p>The experiment must be completed on a laptop or desktop computer with a stable internet connection running the Chrome or Firefox browser. Do not attempt to do it on a smartphone or a tablet. Because one of the tasks involves recording short audio samples, you should make sure that your computer is capable of recording audio before starting the experiment. Most computers have built-in microphones that allow users to make audio recordings. Also, please make sure your browser is configured to allow pop-up notifications.</p>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>Eliminating Distractions</b></legend>'+
    '<p>This experiment collects valuable data for a dissertation research project. It is therefore important that you give the tasks your full attention and that you make sure you are in a quiet and distraction-free location before getting started. Please close all unrelated browser windows and refrain from using resources such as dictionaries and search engines while you are participating. If necessary, you can exit the experiment now and log back in when you have found a more suitable place to participate.</p>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>Participant Compensation</b></legend>'+
    '<p>I am offering 360 TWD as a reward for completing the experiment. At the end of the final task, you will be asked to provide the following information so that I can transfer the funds to your Taiwanese bank account: (1) name of bank, (2) name of account holder, (3) account number. I do my best to process payments within 72 hours of receiving participant submissions. You should feel confident that your submission will be accepted as long as you read the instructions carefully and complete each task to the best of your ability. Feel free to contact me at fzenker@hawaii.edu if you have questions about participant compensation.</p></fieldset><br>'
;
var ko_overview_text = 
    '<h3 style="text-align:center">영어와 한국어에 관한 언어 실험</h3>'+
    '<p>안녕하세요. 실험에 참가하여 주셔서 감사합니다. 본 실험에서는 언어적 배경에 대한 설문지와 문장 읽기와 문제에 답하기 등을 포함한 언어와 관련된 과제를 수행할 것입니다. </p>'+
    '<fieldset><legend><b>실험 설명</b></legend>'+
    '<p>본 실험은 다섯 개의 과제로 이루어져 있습니다. 실험 전체를 한 번에 다 끝낼 수도 있고, 각 과제 하나씩 나눠서 할 수도 있습니다. 과제 전체는 시작 후 7일 이내에 완료하여 주시기 바랍니다. 각 과제의 마지막 부분에서 다음 과제로 진행할 수 있는 새로운 과제ID가 주어질 것입니다. 실험 전체는 60&ndash;90분 정도 소요될 것이라 예상됩니다. 실험 전체를 완료한 후, 실험 완료 설문조사를 제출하시면 연구자에게 자동으로 알림이 보내질 것입니다.</p>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>실험 참가 자격 요건</b></legend>'+
    '<ul>'+
    '<li>만 18세 이상</li>'+
    '<li>한국어 원어민</li>'+
    '<li>영어를 만 8세 이후에 처음 공부하기 시작한 경우</li>'+
    '</ul>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>필요 기기</b></legend>'+
    '<p>본 실험은 크롬이나 파이어폭스 브라우저로 노트북이나 컴퓨터로만 접속하실 수 있습니다 (스마트폰이나 태블릿 사용 불가). 한 과제는 짧은 녹음을 답변으로 제출해야 하기 때문에 실험 시작 전 컴퓨터로 녹음이 가능한 지 확인하여 주시기 바랍니다. 대부분의 컴퓨터는 마이크가 내장되어 있어 녹음이 가능할 것으로 예상됩니다.</p>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>방해 요소 제거</b></legend>'+
    '<p>본 실험은 논문 연구에 매우 중요한 자료를 수집하는 것을 목적으로 합니다. 그러므로 과제를 수행하실 때 최대한 집중하실 수 있도록 방해 요소가 없는 조용한 장소에서 참여하여 주시기 바랍니다. 실험 외의 다른 브라우저나 프로그램은 닫아 주시기 바랍니다. 더하여, 실험 중 검색이나 사전 등 다른 자료를 사용하는 것을 자제하여 주십시오. 필요하시다면, 실험을 할 수 있는 적합한 장소에서 다시 접속하여 주시기 바랍니다.</p>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>실험 참가 사례금</b></legend>'+
    '<p>실험에 참가하시는 분들께 15,000원의 사례금을 드립니다. 사례금은 한국의 계좌로 이체될 예정이며, 실험 마지막 부분에 (1) 은행명, (2) 예금주 명, (3) 계좌번호를 작성하여 주시면 해당 계좌로 송금될 것입니다. 더하여 송금 관련하여 연구자가 연락을 드릴 수 있도록 이메일 주소도 남겨주시면 감사하겠습니다. 실험 참가비는 72시간 이내에 지급될 수 있도록 최선을 다하겠습니다. 사례금 관련하여 질문사항이 있으시면 저에게 이메일 fzenker@hawaii.edu로 연락주시기 바랍니다.</p></fieldset><br>'
;
var zh_overview_text = 
    '<h3 style="text-align:center">中英文語言實驗</h3>'+
    '<fieldset><legend><b>流程說明</b></legend>'+
    '<p>這個實驗一共包含五項測驗，目的在於了解人們如何用他們的母語或/和第二語言處理某些類型的句子。您可以選擇一次做完所有測驗，或者在七天內分批做完各項測驗。在每個測驗的最後，您會收到一組新的測驗序號。透過這組序號，您可以繼續下一個測驗。完成整個實驗大概需要60&ndash;90分鐘。</p>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>參加資格</b></legend>'+
    '<ul>'+
    '<li>18歲以上</li>'+
    '<li>中文母語者</li>'+
    '<li>8歲以後才開始學習英文</li>'+
    '</ul>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>設備條件</b></legend>'+
    '<p>整個實驗必須使用能連接穩定網路的筆電或桌電，而且要能開啟Google或Firefox瀏覽器。請務必不要使用手機或平板來完成實驗。請確認您使用的設備可以錄音，因為其中一項測驗將會需要您錄製簡短的音檔。大多數的電腦都有內建的麥克風可以錄音。另外，請確認您的瀏覽器可以顯示跳出式視窗。</p>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>減少分心的事物</b></legend>'+
    '<p>這個實驗搜集的數據會拿來當博士論文的研究資料，因此需要您在參與實驗時能有高度的專心。例如，做實驗的環境請務必安靜且沒有干擾；關掉所有不相關的視窗；實驗中不要使用任何字典或網頁。如果有必要，您可以登出實驗，找到理想無干擾的環境再繼續參加實驗。</p>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>實驗報酬</b></legend>'+
    '<p>我將會提供完整完成實驗的受試者新台幣360元當作報酬。在最後一個測驗之後，系統會請您提供以下資訊，以助我匯款到您台灣的銀行帳戶：(1)銀行名稱，（2）收款人姓名，（3）銀行帳號。我會盡力在收到您的資料後72小時內完成匯款。如果您仔細遵守實驗說明，並盡力完成每一項測驗，那麼您可以預期繳交的資料被受理。如果您對轉帳資訊有任何疑慮，歡迎寄信到fzenker@hawaii.edu （中英文皆可）。</p></fieldset><br>'
;
var ko_overview_text_lbc = 
    '<h3 style="text-align:center">영어와 한국어에 관한 언어 실험</h3>'+
    '<p>안녕하세요. 실험에 참가하여 주셔서 감사합니다. 본 실험에서는 언어적 배경에 대한 설문지와 문장 읽기와 문제에 답하기 등을 포함한 언어와 관련된 과제를 수행할 것입니다. </p>'+
    '<fieldset><legend><b>실험 설명</b></legend>'+
    '<p>본 실험은 다섯 개의 과제로 이루어져 있습니다. 실험 전체를 한 번에 다 끝낼 수도 있고, 각 과제 하나씩 나눠서 할 수도 있습니다. 과제 전체는 시작 후 7일 이내에 완료하여 주시기 바랍니다. 각 과제의 마지막 부분에서 다음 과제로 진행할 수 있는 새로운 과제ID가 주어질 것입니다. 실험 전체는 60&ndash;90분 정도 소요될 것이라 예상됩니다. 실험 전체를 완료한 후, 실험 완료 설문조사를 제출하시면 연구자에게 자동으로 알림이 보내질 것입니다.</p>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>실험 참가 자격 요건</b></legend>'+
    '<ul>'+
    '<li>만 18세 이상</li>'+
    '<li>한국어 원어민</li>'+
    '</ul>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>필요 기기</b></legend>'+
    '<p>본 실험은 크롬이나 파이어폭스 브라우저로 노트북이나 컴퓨터로만 접속하실 수 있습니다 (스마트폰이나 태블릿 사용 불가). 한 과제는 짧은 녹음을 답변으로 제출해야 하기 때문에 실험 시작 전 컴퓨터로 녹음이 가능한 지 확인하여 주시기 바랍니다. 대부분의 컴퓨터는 마이크가 내장되어 있어 녹음이 가능할 것으로 예상됩니다.</p>'+
    '</fieldset><br>'+
    '<fieldset><legend><b>방해 요소 제거</b></legend>'+
    '<p>본 실험은 논문 연구에 매우 중요한 자료를 수집하는 것을 목적으로 합니다. 그러므로 과제를 수행하실 때 최대한 집중하실 수 있도록 방해 요소가 없는 조용한 장소에서 참여하여 주시기 바랍니다. 실험 외의 다른 브라우저나 프로그램은 닫아 주시기 바랍니다. 더하여, 실험 중 검색이나 사전 등 다른 자료를 사용하는 것을 자제하여 주십시오. 필요하시다면, 실험을 할 수 있는 적합한 장소에서 다시 접속하여 주시기 바랍니다.</p>'+
    '</fieldset><br>'
;

var en_overview_stimulus =
    overview_header+en_overview_text
;
var ko_overview_stimulus =
    overview_header+ko_overview_text
;
var zh_overview_stimulus =
    overview_header+zh_overview_text
;
var ko_overview_stimulus_lbc =
    overview_header+ko_overview_text_lbc
;

// *** consent form ***

var welcome_header =
    '<style>p {text-align:left;} .section {padding:.5em;} table {border-collapse:collapse; margin-left:auto; margin-right:auto;} .row {border:3px solid #c6c6c6;box-shadow:2px 2px 5px #999; padding:.5em;} td {text-align:left;} </style>'+
    '<img src="img_overview.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)"><br>'
;

var consent_header =
    '<style>p {text-align:left;} .section {padding:.5em;} table {border-collapse:collapse; margin-left:auto; margin-right:auto;} .row {border:3px solid #c6c6c6;box-shadow:2px 2px 5px #999; padding:.5em;} td {text-align:left;} </style>'+
    '<img src="img_consent.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)"><br>'
;

var en_consent_text =
    '<span style="font-size:125%; font-weight:bold">Reading, Rating, and Recording Sentences in English</span>'+
    '<p>Welcome! In this experiment, you will complete a language background survey and four language tasks that involve reading, rating, and recording sentences in English. Your responses will provide important data for a dissertation research project.</p>'+
    '<p>Before we get started with the experiment, it is important that you know your rights as a participant. Click on the link below to open the consent form in a new tab on your browser.</p>'+
    '<a href="consent-form-english-v3.pdf" target="_blank" style="font-size:115%;" >Click here to read the consent form</a><br><br>'+
    '<table>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="agreement1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>I have read and understood all the contents of the consent form, and I agree to participate in the experiment</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="agreement2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>I agree to read all the instructions carefully and to give each task my best effort</p>'+
    '</div></td></tr>'+
    '</table><br>'
;
var ko_consent_text =
    '<span style="font-size:125%; font-weight:bold">사전동의</span>'+
    '<p>실험에 참여하기 전 참가자로서의 권리를 알려드립니다. 아래의 링크를 클릭하시면 사전 동의서의 전문을 보실 수 있습니다.</p>'+
    '<a href="consent-form-korean-v3.pdf" target="_blank">사전동의서</a><br><br>'+
    '<table>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="agreement1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>실험 참가 동의서의 내용을 읽고 이해하였으며 실험에 참여하는 것에 동의합니다.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="agreement2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>실험에 대한 설명을 모두 읽었으며 각 질문에 최선을 다해 답변할 것에 동의합니다.</p>'+
    '</div></td></tr>'+
    '</table><br>'
;
var zh_consent_text =
    '<span style="font-size:125%; font-weight:bold">同意書說明</span>'+
    '<p>在開始實驗前，我們將向您說明您作為受試者的權利。請點選以下連結在新的分頁中開啟同意書。</p>'+
    '<a href="consent-form-mandarin-v3.pdf" target="_blank" style="font-size:150%">同意書</a><br><br>'+
    '<table>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="agreement1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>我已閱讀並了解所有同意書的內容，並同意參與實驗。</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="agreement2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>我將仔細閱讀所有實驗相關的指示，並盡我所能依循。</p>'+
    '</div></td></tr>'+
    '</table><br>'
;
var lbc_consent_text =
    '<span style="font-size:125%; font-weight:bold">Informed Consent</span>'+
    '<p>Before we get started with the experiment, it is important that you know your rights as a participant. Click on the link below to open the consent form in a new tab on your browser.</p>'+
    '<a href="consent-form-english-v3.pdf" target="_blank" style="font-size:115%;" >Click here to read the consent form</a><br><br>'+
    '<table>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="agreement1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>I have read and understood all the contents of the consent form, and I agree to participate in the experiment</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="agreement2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>I agree to read all the instructions carefully and to give each task my best effort</p>'+
    '</div></td></tr>'+
    '</table><br>'
;

var en_consent_html =
    welcome_header+en_consent_text
;
var ko_consent_html =
    consent_header+ko_consent_text
;
var zh_consent_html =
    consent_header+zh_consent_text
;
var lbc_consent_html =
    consent_header+lbc_consent_text
;

// *** language survey ***

var number_options =
    '<option value="1">1</option><option value="2">2</option><option value="3">3</option><option value="4">4</option><option value="5">5</option><option value="6">6</option><option value="7">7</option><option value="8">8</option><option value="9">9</option><option value="10">10</option><option value="11">11</option><option value="12">12</option><option value="13">13</option><option value="14">14</option><option value="15">15</option><option value="16">16</option><option value="17">17</option><option value="18">18</option><option value="19">19</option><option value="20">20</option><option value="21">21</option><option value="22">22</option><option value="23">23</option><option value="24">24</option><option value="25">25</option><option value="26">26</option><option value="27">27</option><option value="28">28</option><option value="29">29</option><option value="30">30</option><option value="31">31</option><option value="32">32</option><option value="33">33</option><option value="34">34</option><option value="35">35</option><option value="36">36</option><option value="37">37</option><option value="38">38</option><option value="39">39</option><option value="40">40</option><option value="41">41</option><option value="42">42</option><option value="43">43</option><option value="44">44</option><option value="45">45</option><option value="46">46</option><option value="47">47</option><option value="48">48</option><option value="49">49</option><option value="50">50</option><option value="51">51</option><option value="52">52</option><option value="53">53</option><option value="54">54</option><option value="55">55</option><option value="56">56</option><option value="57">57</option><option value="58">58</option><option value="59">59</option><option value="60">60</option><option value="61">61</option><option value="62">62</option><option value="63">63</option><option value="64">64</option><option value="65">65</option><option value="66">66</option><option value="67">67</option><option value="68">68</option><option value="69">69</option><option value="70">70</option><option value="71">71</option><option value="72">72</option><option value="73">73</option><option value="74">74</option><option value="75">75</option><option value="76">76</option><option value="77">77</option><option value="78">78</option><option value="79">79</option><option value="80">80</option><option value="81">81</option><option value="82">82</option><option value="83">83</option><option value="84">84</option><option value="85">85</option><option value="86">86</option><option value="87">87</option><option value="88">88</option><option value="89">89</option><option value="90">90</option><option value="91">91</option><option value="92">92</option><option value="93">93</option><option value="94">94</option><option value="95">95</option><option value="96">96</option><option value="97">97</option><option value="98">98</option><option value="99">99</option>'
;

var en_age_options =
    number_options
;
var ko_age_options =
    '<option value="1">만 1세</option><option value="2">만 2세</option><option value="3">만 3세</option><option value="4">만 4세</option><option value="5">만 5세</option><option value="6">만 6세</option><option value="7">만 7세</option><option value="8">만 8세</option><option value="9">만 9세</option><option value="10">만 10세</option><option value="11">만 11세</option><option value="12">만 12세</option><option value="13">만 13세</option><option value="14">만 14세</option><option value="15">만 15세</option><option value="16">만 16세</option><option value="17">만 17세</option><option value="18">만 18세</option><option value="19">만 19세</option><option value="20">만 20세</option><option value="21">만 21세</option><option value="22">만 22세</option><option value="23">만 23세</option><option value="24">만 24세</option><option value="25">만 25세</option><option value="26">만 26세</option><option value="27">만 27세</option><option value="28">만 28세</option><option value="29">만 29세</option><option value="30">만 30세</option><option value="31">만 31세</option><option value="32">만 32세</option><option value="33">만 33세</option><option value="34">만 34세</option><option value="35">만 35세</option><option value="36">만 36세</option><option value="37">만 37세</option><option value="38">만 38세</option><option value="39">만 39세</option><option value="40">만 40세</option><option value="41">만 41세</option><option value="42">만 42세</option><option value="43">만 43세</option><option value="44">만 44세</option><option value="45">만 45세</option><option value="46">만 46세</option><option value="47">만 47세</option><option value="48">만 48세</option><option value="49">만 49세</option><option value="50">만 50세</option><option value="51">만 51세</option><option value="52">만 52세</option><option value="53">만 53세</option><option value="54">만 54세</option><option value="55">만 55세</option><option value="56">만 56세</option><option value="57">만 57세</option><option value="58">만 58세</option><option value="59">만 59세</option><option value="60">만 60세</option><option value="61">만 61세</option><option value="62">만 62세</option><option value="63">만 63세</option><option value="64">만 64세</option><option value="65">만 65세</option><option value="66">만 66세</option><option value="67">만 67세</option><option value="68">만 68세</option><option value="69">만 69세</option><option value="70">만 70세</option><option value="71">만 71세</option><option value="72">만 72세</option><option value="73">만 73세</option><option value="74">만 74세</option><option value="75">만 75세</option><option value="76">만 76세</option><option value="77">만 77세</option><option value="78">만 78세</option><option value="79">만 79세</option><option value="80">만 80세</option><option value="81">만 81세</option><option value="82">만 82세</option><option value="83">만 83세</option><option value="84">만 84세</option><option value="85">만 85세</option><option value="86">만 86세</option><option value="87">만 87세</option><option value="88">만 88세</option><option value="89">만 89세</option><option value="90">만 90세</option><option value="91">만 91세</option><option value="92">만 92세</option><option value="93">만 93세</option><option value="94">만 94세</option><option value="95">만 95세</option><option value="96">만 96세</option><option value="97">만 97세</option><option value="98">만 98세</option><option value="99">만 99세</option>'
;

var language_survey_header =
    '<style>fieldset {border:1px solid #999;box-shadow:2px 2px 5px #999;} legend {background:#fff;text-align:left;font-size:110%;} p {text-align:left;} h3 {text-align:left;font-size:110%;} li {margin-bottom:10px} ol {text-align:left}</style>'+
    '<img src="img_lbq.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)"><br>'
;

var en_language_survey_text =
    // header
    '<span style="text-align:center;font-weight:bold;font-size:125%;">Language Background Survey</span>'+
    '<p style="text-align:center;">This form asks for some important information about your language background</p>'+
    // basic information
    '<fieldset><legend><b>Basic Information</b></legend>'+
    '<ol>'+
    '<li>Age: <input type="number" name="age" min="1", max="100" required style="width: 6ch"></li>'+
    '<li>Gender: <input type="radio" name="gender" value="female" required>Female <input type="radio" name="gender" value="male">Male <input type="radio" name="gender" value="nonbinary">Nonbinary</li>'+
    '<li>Place where you grew up: <input name="state-where-you-grew-up" type="text" style="width: 15ch" placeholder="State/Province" required>, <input name="country-where-you-grew-up" type="text" style="width: 15ch" placeholder="Country" required></li>'+
    '<li>Place where you live now: <input name="state-where-you-live-now" type="text" style="width: 15ch" placeholder="State/Province" required>, <input name="country-where-you-live-now" type="text" style="width: 15ch" placeholder="Country" required></li>'+
    '<li>Highest level of education completed:<br><input type="radio" name="education" value="highschool" required>High School Diploma <input type="radio" name="education" value="undergrad">Undergraduate Degree <input type="radio" name="education" value="graduate">Graduate Degree <input type="radio" name="education" value="other">Other</li>'+
    '</ol>'+
    '</fieldset><br>'+
    // places you have lived
    '<fieldset><legend><b>Places Where You Have Lived</b></legend>'+
    '<p>Let us know about the countries you have lived in for a year or longer, including your home country</p>'+
    // country1
    '<h3>Country 1 (Required):</h3>'+
    '<ol>'+
    '<li>Name of country: <input name="country1_name" type="text" size="10"></li>'+
    '<li>Age when you moved to this country: <select name="country1_arrival" required><option value=""></option><option value="NA">Not Applicable</option><option value="0">Born in This Country</option>'+number_options+'</select></li>'+
    '<li>Total number of years spent living in this country: <select name="country1_duration" required><option value=""></option><option value="NA">Not Applicable</option>'+number_options+'</select></li>'+
    '<li>Age when you moved away (if applicable): <select name="country1_departure" required><option value=""></option><option value="NA">Not Applicable</option>'+number_options+'</select></li>'+
    '</ol>'+
    // country2
    '<h3>Country 2 (If Applicable):</h3>'+
    '<ol>'+
    '<li>Name of country: <input name="country2_name" type="text" size="10"></li>'+
    '<li>Age when you moved to this country: <select name="country2_arrival"><option value=""></option><option value="NA">Not Applicable</option><option value="0">Born in This Country</option>'+number_options+'</select></li>'+
    '<li>Total number of years spent living in this country: <select name="country2_duration"><option value=""></option><option value="NA">Not Applicable</option>'+number_options+'</select></li>'+
    '<li>Age when you moved away (if applicable): <select name="country2_departure"><option value=""></option><option value="NA">Not Applicable</option>'+number_options+'</select></li>'+
    '</ol>'+
    // country3
    '<h3>Country 3 (If Applicable):</h3>'+
    '<ol>'+
    '<li>Name of country: <input name="country3_name" type="text" size="10"></li>'+
    '<li>Age when you moved to this country: <select name="country3_arrival"><option value=""></option><option value="NA">Not Applicable</option><option value="0">Born in This Country</option>'+number_options+'</select></li>'+
    '<li>Total number of years spent living in this country: <select name="country3_duration"><option value=""></option><option value="NA">Not Applicable</option>'+number_options+'</select></li>'+
    '<li>Age when you moved away (if applicable): <select name="country3_departure"><option value=""></option><option value="NA">Not Applicable</option>'+number_options+'</select></li>'+
    '</ol>'+
    // other
    '<h3>If you have lived in any additional countries, tell us about them here:</h3>'+
    '<p style="text-align:left;"><textarea name="other_countries" rows="3" style="width:90%;"></textarea></p>'+
    '</fieldset><br>'+
    // languages you speak
    '<fieldset><legend><b>Languages You Speak</b></legend>'+
    '<p>Provide information about all the languages you speak, including your native language</p>'+
    // english
    '<h3>English (Required):</h3>'+
    '<ol>'+
    '<li>Proficiency level: <select name="english_proficiency" required><option value=""></option><option value="beginner">1 - Beginner</option><option value="lowintermediate">2 - Low Intermediate</option><option value="highintermediate">3 - High Intermediate</option><option value="advanced">4 - Advanced</option><option value="nearnative">5 - Near-Native</option><option value="native">6 - Native Speaker</option></select></li>'+
    '<li>Age when you started learning this language: <select name="english_aoa" required><option value=""></option><option value="0">From Birth</option>'+number_options+'</select></li>'+
    '<li>Did your primary caregivers speak this language to you when you were a child? <input type="radio" name="english_heritage" value="yes" required>Yes <input type="radio" name="english_heritage" value="no">No</li>'+
    '<li>Percentage of total weekly listening/speaking/reading/writing time spent using this language: <input type="number" min="0" max="100" name="english_percentage"  style="width: 6ch" required>%</li>'+
    '<li>Number of years spent studying this language as a second or foreign language (if applicable): <select name="english_duration" required><option value=""></option><option value="NA">Not Applicable</option>'+number_options+'</select></li>'+
    '<li>Have you taken any advanced courses on English syntax (600-level or above?) <input type="radio" name="english_syntax" value="yes" required>Yes <input type="radio" name="english_syntax" value="no">No</li>'+
    '</ol>'+
    // language2
    '<h3>Language 2 (If Applicable):</h3>'+
    '<ol>'+
    '<li>Name of language: <input name="language2_name" type="text" size="10"></li>'+
    '<li>Proficiency level: <select name="language2_proficiency"><option value=""></option><option value="beginner">1 - Beginner</option><option value="lowintermediate">2 - Low Intermediate</option><option value="highintermediate">3 - High Intermediate</option><option value="advanced">4 - Advanced</option><option value="nearnative">5 - Near-Native</option><option value="native">6 - Native Speaker</option></select></li>'+
    '<li>Age when you started learning this language: <select name="language2_aoa"><option value=""></option><option value="0">From Birth</option>'+number_options+'</select></li>'+
    '<li>Did your primary caregivers speak this language to you when you were a child? <input type="radio" name="language2_heritage" value="yes" >Yes <input type="radio" name="language2_heritage" value="no" >No</li>'+
    '<li>Percentage of total weekly listening/speaking/reading/writing time spent using this language: <input type="number" min="0" max="100" name="language2_percentage"  style="width: 6ch">%</li>'+
    '<li>Number of years spent studying this language as a second or foreign language (if applicable): <select name="language2_duration"><option value=""></option><option value="NA">Not Applicable</option>'+number_options+'</select></li>'+
    '</ol>'+
    // language3
    '<h3>Language 3 (If Applicable):</h3>'+
    '<ol>'+
    '<li>Name of language: <input name="language3_name" type="text" size="10"></li>'+
    '<li>Proficiency level: <select name="language3_proficiency"><option value=""></option><option value="beginner">1 - Beginner</option><option value="lowintermediate">2 - Low Intermediate</option><option value="highintermediate">3 - High Intermediate</option><option value="advanced">4 - Advanced</option><option value="nearnative">5 - Near-Native</option><option value="native">6 - Native Speaker</option></select></li>'+
    '<li>Age when you started learning this language: <select name="language3_aoa"><option value=""></option><option value="0">From Birth</option>'+number_options+'</select></li>'+
    '<li>Did your primary caregivers speak this language to you when you were a child? <input type="radio" name="language3_heritage" value="yes" >Yes <input type="radio" name="language3_heritage" value="no" >No</li>'+
    '<li>Percentage of total weekly listening/speaking/reading/writing time spent using this language: <input type="number" min="0" max="100" name="language3_percentage"  style="width: 6ch">%</li>'+
    '<li>Number of years spent studying this language as a second or foreign language (if applicable): <select name="language3_duration"><option value=""></option><option value="NA">Not Applicable</option>'+number_options+'</select></li>'+
    '</ol>'+
    // other
    '<h3>If you speak any additional languages, tell us about them here:</h3>'+
    '<p style="text-align:left;"><textarea name="other_languages" rows="3" style="width:90%;"></textarea></p>'+
    '</fieldset><br>'
;
var ko_language_survey_text =
    // header
    '<span style="text-align:center;font-weight:bold;font-size:125%;">언어 배경 조사</span>'+
    '<p style="text-align:center;">실험 참여 자격을 확인하기 위해 아래의 설문조사에 답변하고 언어 배경에 대한 정보를 제공하여 주시기 바랍니다.</p>'+
    // basic information
    '<fieldset><legend><b>기본 정보</b></legend>'+
    '<ol>'+
    '<li>만 나이: <input type="number" name="age" min="1", max="100" required style="width: 6ch"></li>'+
    '<li>성별: <input type="radio" name="gender" value="female" required>여성 <input type="radio" name="gender" value="male">남성 <input type="radio" name="gender" value="nonbinary">기타</li>'+
    '<li>자란 곳: <input name="state-where-you-grew-up" type="text" style="width: 15ch" placeholder="주/도" required>, <input name="country-where-you-grew-up" type="text" style="width: 15ch" placeholder="나라" required></li>'+
    '<li>사는 곳: <input name="state-where-you-live-now" type="text" style="width: 15ch" placeholder="주/도" required>, <input name="country-where-you-live-now" type="text" style="width: 15ch" placeholder="나라" required></li>'+
    '<li>최종 학력: <input type="radio" name="education" value="highschool" required>고등학교 <input type="radio" name="education" value="undergrad">대학교 <input type="radio" name="education" value="graduate">대학원 <input type="radio" name="education" value="other">기타</li>'+
    '</ol>'+
    '</fieldset><br>'+
    // places you have lived
    '<fieldset><legend><b>거주 국가</b></legend>'+
    '<p>1년 이상 거주했던 국가를 알려주십시오.</p>'+
    // korea
    '<h3>한국 (필수):</h3>'+
    '<ol>'+
    '<li>이 국가에 처음 거주하기 시작한 나이(만): <select name="korea_arrival" required><option value=""></option><option value="NA">해당 사항 없음</option><option value="0">태어날 때부터</option>'+ko_age_options+'</select></li>'+
    '<li>이 국가에서 거주한 연수: <select name="korea_duration" required><option value=""></option><option value="NA">해당 사항 없음</option>'+number_options+'</select></li>'+
    '<li>이 국가를 떠난 나이(만): <select name="korea_departure" required><option value=""></option><option value="NA">해당 사항 없음</option>'+ko_age_options+'</select></li>'+
    '</ol>'+
    // country2
    '<h3>제2국 (해당 사항이 있는 경우):</h3>'+
    '<ol>'+
    '<li>국가 명: <input name="country2_name" type="text" size="10"></li>'+
    '<li>이 국가에 처음 거주하기 시작한 나이(만): <select name="country2_arrival"><option value=""></option><option value="NA">해당 사항 없음</option><option value="0">태어날 때부터</option>'+ko_age_options+'</select></li>'+
    '<li>이 국가에서 거주한 연수: <select name="country2_duration"><option value=""></option><option value="NA">해당 사항 없음</option>'+number_options+'</select></li>'+
    '<li>이 국가를 떠난 나이(만): <select name="country2_departure"><option value=""></option><option value="NA">해당 사항 없음</option>'+ko_age_options+'</select></li>'+
    '</ol>'+
    // country3
    '<h3>제3국 (해당 사항이 있는 경우):</h3>'+
    '<ol>'+
    '<li>국가 명: <input name="country3_name" type="text" size="10"></li>'+
    '<li>이 국가에 처음 거주하기 시작한 나이(만): <select name="country3_arrival"><option value=""></option><option value="NA">해당 사항 없음</option><option value="0">태어날 때부터</option>'+ko_age_options+'</select></li>'+
    '<li>이 국가에서 거주한 연수: <select name="country3_duration"><option value=""></option><option value="NA">해당 사항 없음</option>'+number_options+'</select></li>'+
    '<li>이 국가를 떠난 나이(만): <select name="country3_departure"><option value=""></option><option value="NA">해당 사항 없음</option>'+ko_age_options+'</select></li>'+
    '</ol>'+
    // other
    '<h3>그 외 다른 국가에서 거주한 경험이 있다면 알려주십시오:</h3>'+
    '<p style="text-align:left;"><textarea name="other_countries" rows="3" style="width:90%;"></textarea></p>'+
    '</fieldset><br>'+
    // languages you speak
    '<fieldset><legend><b>사용하는 언어</b></legend>'+
    '<p>사용하는 모든 언어에 관하여 정보를 제공하여 주시기 바랍니다.</p>'+
    // korean
    '<h3>한국어 (필수):</h3>'+
    '<ol>'+
    '<li>능력 수준: <select name="korean_proficiency" required><option value=""></option><option value="beginner">1 - 초보</option><option value="lowintermediate">2 - 중하</option><option value="highintermediate">3 - 중상</option><option value="advanced">4 - 고급</option><option value="nearnative">5 - 모국어 수준에 근접</option><option value="native">6 - 원어민</option></select></li>'+
    '<li>이 언어를 처음 배우기 시작한 나이 (만): <select name="korean_aoa" required><option value=""></option><option value="0">태어날 때부터</option>'+ko_age_options+'</select></li>'+
    '<li>어렸을 때 주양육자(부모님, 친척 등)가 귀하에게 이 언어를 사용하였습니까? <input type="radio" name="korean_heritage" value="yes" required>네 <input type="radio" name="korean_heritage" value="no">아니오</li>'+
    '<li>한 주간 이 언어를 듣기/말하기/읽기/쓰기에 사용하는 시간의 비율: <input type="number" min="0" max="100" name="korean_percentage" style="width: 6ch" required>%</li>'+
    '<li>이 언어를 제2언어 또는 외국어로 공부한 경우, 해당 년수를 작성하여 주십시오: <select name="korean_duration" required><option value=""></option><option value="NA">해당 사항 없음</option>'+number_options+'</select></li>'+
    '</ol>'+
    // english
    '<h3>영어 (필수):</h3>'+
    '<ol>'+
    '<li>능력 수준: <select name="english_proficiency" required><option value=""></option><option value="beginner">1 - 초보</option><option value="lowintermediate">2 - 중하</option><option value="highintermediate">3 - 중상</option><option value="advanced">4 - 고급</option><option value="nearnative">5 - 모국어 수준에 근접</option><option value="native">6 - 원어민</option></select></li>'+
    '<li>이 언어를 처음 배우기 시작한 나이 (만): <select name="english_aoa" required><option value=""></option><option value="0">태어날 때부터</option>'+ko_age_options+'</select></li>'+
    '<li>어렸을 때 주양육자(부모님, 친척 등)가 귀하에게 이 언어를 사용하였습니까? <input type="radio" name="english_heritage" value="yes" required>네 <input type="radio" name="english_heritage" value="no">아니오</li>'+
    '<li>한 주간 이 언어를 듣기/말하기/읽기/쓰기에 사용하는 시간의 비율: <input type="number" min="0" max="100" name="english_percentage"  style="width: 6ch" required>%</li>'+
    '<li>이 언어를 제2언어 또는 외국어로 공부한 경우, 해당 년수를 작성하여 주십시오: <select name="english_duration" required><option value=""></option><option value="NA">해당 사항 없음</option>'+number_options+'</select></li>'+
    '</ol>'+
    // language3
    '<h3>제3언어 (해당 사항이 있는 경우):</h3>'+
    '<ol>'+
    '<li>언어 명: <input name="language3_name" type="text" size="10"></li>'+
    '<li>능력 수준: <select name="language3_proficiency"><option value=""></option><option value="beginner">1 - 초보</option><option value="lowintermediate">2 - 중하</option><option value="highintermediate">3 - 중상</option><option value="advanced">4 - 고급</option><option value="nearnative">5 - 모국어 수준에 근접</option><option value="native">6 - 원어민</option></select></li>'+
    '<li>이 언어를 처음 배우기 시작한 나이 (만): <select name="language3_aoa"><option value=""></option><option value="0">태어날 때부터</option>'+ko_age_options+'</select></li>'+
    '<li>어렸을 때 주양육자(부모님, 친척 등)가 귀하에게 이 언어를 사용하였습니까? <input type="radio" name="language3_heritage" value="yes" >네 <input type="radio" name="language3_heritage" value="no" >아니오</li>'+
    '<li>한 주간 이 언어를 듣기/말하기/읽기/쓰기에 사용하는 시간의 비율: <input type="number" min="0" max="100" name="language3_percentage"  style="width: 6ch">%</li>'+
    '<li>이 언어를 제2언어 또는 외국어로 공부한 경우, 해당 년수를 작성하여 주십시오: <select name="language3_duration"><option value=""></option><option value="NA">해당 사항 없음</option>'+number_options+'</select></li>'+
    '</ol>'+
    // language4
    '<h3>제4언어 (해당 사항이 있는 경우):</h3>'+
    '<ol>'+
    '<li>언어 명: <input name="language4_name" type="text" size="10"></li>'+
    '<li>능력 수준: <select name="language4_proficiency"><option value=""></option><option value="beginner">1 - 초보</option><option value="lowintermediate">2 - 중하</option><option value="highintermediate">3 - 중상</option><option value="advanced">4 - 고급</option><option value="nearnative">5 - 모국어 수준에 근접</option><option value="native">6 - 원어민</option></select></li>'+
    '<li>이 언어를 처음 배우기 시작한 나이 (만): <select name="language4_aoa"><option value=""></option><option value="0">태어날 때부터</option>'+ko_age_options+'</select></li>'+
    '<li>어렸을 때 주양육자(부모님, 친척 등)가 귀하에게 이 언어를 사용하였습니까? <input type="radio" name="language4_heritage" value="yes" >네 <input type="radio" name="language4_heritage" value="no" >아니오</li>'+
    '<li>한 주간 이 언어를 듣기/말하기/읽기/쓰기에 사용하는 시간의 비율: <input type="number" min="0" max="100" name="language4_percentage"  style="width: 6ch">%</li>'+
    '<li>이 언어를 제2언어 또는 외국어로 공부한 경우, 해당 년수를 작성하여 주십시오: <select name="language4_duration"><option value=""></option><option value="NA">해당 사항 없음</option>'+number_options+'</select></li>'+
    '</ol>'+
    // other
    '<h3>추가로 다른 언어를 사용하는 경우 알려주십시오:</h3>'+
    '<p style="text-align:left;"><textarea name="other_languages" rows="3" style="width:90%;"></textarea></p>'+
    '</fieldset><br>'
;
var zh_language_survey_text =
    // header
    '<span style="text-align:center;font-weight:bold;font-size:125%;">語言背景調查</span>'+
    '<p style="text-align:center;">請回答以下關於您語言背景的問題，並確認您符合參與實驗的資格要求。</p>'+
    // basic information
    '<fieldset><legend><b>基本資訊</b></legend>'+
    '<ol>'+
    '<li>年齡： <input type="number" name="age" min="1", max="100" required style="width: 6ch"></li>'+
    '<li>性別： <input type="radio" name="gender" value="female" required>女性 <input type="radio" name="gender" value="male">男性 <input type="radio" name="gender" value="nonbinary">非二元</li>'+
    '<li>成長地： <input name="state-where-you-grew-up" type="text" style="width: 15ch" placeholder="縣市" required>, <input name="country-where-you-grew-up" type="text" style="width: 15ch" placeholder="國家" required></li>'+
    '<li>現在居住地： <input name="state-where-you-live-now" type="text" style="width: 15ch" placeholder="縣市" required>, <input name="country-where-you-live-now" type="text" style="width: 15ch" placeholder="國家" required></li>'+
    '<li>最高教育程度：<br><input type="radio" name="education" value="highschool" required>高中學歷 <input type="radio" name="education" value="undergrad">大專院校學歷 <input type="radio" name="education" value="graduate">研究所學歷 <input type="radio" name="education" value="other">其他學歷</li>'+
    '</ol>'+
    '</fieldset><br>'+
    // places you have lived
    '<fieldset><legend><b>您居住過的地方</b></legend>'+
    '<p>請提供您居住超過一年或以上的國家，包含您擁有國籍的國家。</p>'+
    // country1
    '<h3>國家1（必填）：</h3>'+
    '<ol>'+
    '<li>國家名稱： <input name="country1_name" type="text" size="10"></li>'+
    '<li>您開始在這個國家居住的年紀： <select name="country1_arrival" required><option value=""></option><option value="NA">不適用</option><option value="0">出生於這個國家</option>'+number_options+'</select></li>'+
    '<li>在這個國家居住的時間（年）： <select name="country1_duration" required><option value=""></option><option value="NA">不適用</option>'+number_options+'</select></li>'+
    '<li>搬離這個國家的年紀（如適用）： <select name="country1_departure" required><option value=""></option><option value="NA">不適用</option>'+number_options+'</select></li>'+
    '</ol>'+
    // country2
    '<h3>國家2（如適用）：</h3>'+
    '<ol>'+
    '<li>國家名稱： <input name="country2_name" type="text" size="10"></li>'+
    '<li>您開始在這個國家居住的年紀： <select name="country2_arrival"><option value=""></option><option value="NA">不適用</option><option value="0">出生於這個國家</option>'+number_options+'</select></li>'+
    '<li>在這個國家居住的時間（年）： <select name="country2_duration"><option value=""></option><option value="NA">不適用</option>'+number_options+'</select></li>'+
    '<li>搬離這個國家的年紀（如適用）： <select name="country2_departure"><option value=""></option><option value="NA">不適用</option>'+number_options+'</select></li>'+
    '</ol>'+
    // country3
    '<h3>國家3（如適用）：</h3>'+
    '<ol>'+
    '<li>國家名稱： <input name="country3_name" type="text" size="10"></li>'+
    '<li>您開始在這個國家居住的年紀： <select name="country3_arrival"><option value=""></option><option value="NA">不適用</option><option value="0">出生於這個國家</option>'+number_options+'</select></li>'+
    '<li>在這個國家居住的時間（年）： <select name="country3_duration"><option value=""></option><option value="NA">不適用</option>'+number_options+'</select></li>'+
    '<li>搬離這個國家的年紀（如適用）： <select name="country3_departure"><option value=""></option><option value="NA">不適用</option>'+number_options+'</select></li>'+
    '</ol>'+
    // other
    '<h3>如果您還居住過更多國家，請在這裡向我們簡述：</h3>'+
    '<p style="text-align:left;"><textarea name="other_countries" rows="3" style="width:90%;"></textarea></p>'+
    '</fieldset><br>'+
    // languages you speak
    '<fieldset><legend><b>您會的語言</b></legend>'+
    '<p>請提供所有關於您會講的語言的相關資訊，包含母語。</p>'+
    // mandarin
    '<h3>中文 (必填)：</h3>'+
    '<ol>'+
    '<li>程度： <select name="mandarin_proficiency" required><option value=""></option><option value="beginner">1 - 初級</option><option value="lowintermediate">2 - 中低級</option><option value="highintermediate">3 - 中高級</option><option value="advanced">4 - 高級</option><option value="nearnative">5 - 近似母語</option><option value="native">6 - 母語</option></select></li>'+
    '<li>開始學習這個語言的年紀： <select name="mandarin_aoa" required><option value=""></option><option value="0">出生開始</option>'+number_options+'</select></li>'+
    '<li>您幼年的主要照顧者和您也使用這個語言嗎？ <input type="radio" name="mandarin_heritage" value="yes" required>是 <input type="radio" name="mandarin_heritage" value="no">否</li>'+
    '<li>每週您花多少時間使用這個語言呢（聽說讀寫）？ <input type="number" min="0" max="100" name="mandarin_percentage"  style="width: 6ch" required>%</li>'+
    '<li>以第二語言或外語學習這個語言的時間 （年；如適用）： <select name="mandarin_duration" required><option value=""></option><option value="NA">不適用</option>'+number_options+'</select></li>'+
    '</ol>'+
    // english
    '<h3>英語 (必填)：</h3>'+
    '<ol>'+
    '<li>程度： <select name="english_proficiency" required><option value=""></option><option value="beginner">1 - 初級</option><option value="lowintermediate">2 - 中低級</option><option value="highintermediate">3 - 中高級</option><option value="advanced">4 - 高級</option><option value="nearnative">5 - 近似母語</option><option value="native">6 - 母語</option></select></li>'+
    '<li>開始學習這個語言的年紀： <select name="english_aoa" required><option value=""></option><option value="0">出生開始</option>'+number_options+'</select></li>'+
    '<li>您幼年的主要照顧者和您也使用這個語言嗎？ <input type="radio" name="english_heritage" value="yes" required>是 <input type="radio" name="english_heritage" value="no">否</li>'+
    '<li>每週您花多少時間使用這個語言呢（聽說讀寫）？ <input type="number" min="0" max="100" name="english_percentage"  style="width: 6ch" required>%</li>'+
    '<li>以第二語言或外語學習這個語言的時間 （年；如適用）： <select name="english_duration" required><option value=""></option><option value="NA">不適用</option>'+number_options+'</select></li>'+
    '<li>您上過任何關於句法(syntax)的課嗎？ <input type="radio" name="english_syntax" value="yes" required>是 <input type="radio" name="english_syntax" value="no">否</li>'+
    '</ol>'+
    // language3
    '<h3>語言3 (如適用)：</h3>'+
    '<ol>'+
    '<li>語言名稱： <input name="language3_name" type="text" size="10"></li>'+
    '<li>程度： <select name="language3_proficiency"><option value=""></option><option value="beginner">1 - 初級</option><option value="lowintermediate">2 - 中低級</option><option value="highintermediate">3 - 中高級</option><option value="advanced">4 - 高級</option><option value="nearnative">5 - 近似母語</option><option value="native">6 - 母語</option></select></li>'+
    '<li>開始學習這個語言的年紀： <select name="language3_aoa"><option value=""></option><option value="0">出生開始</option>'+number_options+'</select></li>'+
    '<li>您幼年的主要照顧者和您也使用這個語言嗎？ <input type="radio" name="language3_heritage" value="yes" >是 <input type="radio" name="language3_heritage" value="no" >否</li>'+
    '<li>每週您花多少時間使用這個語言呢（聽說讀寫）？ <input type="number" min="0" max="100" name="language3_percentage"  style="width: 6ch">%</li>'+
    '<li>以第二語言或外語學習這個語言的時間 （年；如適用）： <select name="language3_duration"><option value=""></option><option value="NA">不適用</option>'+number_options+'</select></li>'+
    '</ol>'+
    // language4
    '<h3>語言4 (如適用)：</h3>'+
    '<ol>'+
    '<li>語言名稱： <input name="language3_name" type="text" size="10"></li>'+
    '<li>程度： <select name="language3_proficiency"><option value=""></option><option value="beginner">1 - 初級</option><option value="lowintermediate">2 - 中低級</option><option value="highintermediate">3 - 中高級</option><option value="advanced">4 - 高級</option><option value="nearnative">5 - 近似母語</option><option value="native">6 - 母語</option></select></li>'+
    '<li>開始學習這個語言的年紀： <select name="language3_aoa"><option value=""></option><option value="0">出生開始</option>'+number_options+'</select></li>'+
    '<li>您幼年的主要照顧者和您也使用這個語言嗎？ <input type="radio" name="language3_heritage" value="yes" >是 <input type="radio" name="language3_heritage" value="no" >否</li>'+
    '<li>每週您花多少時間使用這個語言呢（聽說讀寫）？ <input type="number" min="0" max="100" name="language3_percentage"  style="width: 6ch">%</li>'+
    '<li>以第二語言或外語學習這個語言的時間 （年；如適用） <select name="language3_duration"><option value=""></option><option value="NA">不適用</option>'+number_options+'</select></li>'+
    '</ol>'+
    // other
    '<h3>如果您還會說其他任何語言，請在這裡和我們說明：</h3>'+
    '<p style="text-align:left;"><textarea name="other_languages" rows="3" style="width:90%;"></textarea></p>'+
    '</fieldset><br>'
;

var en_language_survey_html =
    language_survey_header+en_language_survey_text
;
var ko_language_survey_html =
    language_survey_header+ko_language_survey_text
;
var zh_language_survey_html =
    language_survey_header+zh_language_survey_text
;

var en_language_survey_feedback_stimulus =
    '<style>p {text-align:left;} li {margin-bottom:10px} ul {text-align:left}</style>'+
    '<img src="img_warning.png" height="150" title="image credit: Flaticon / Freepik (www.freepik.com)"><br>'+
    '<h3 style="text-align:center">Notice of Ineligibility to Participate</h3>'+
    '<p>According to the responses you gave, it appears that you are not eligible to participate in the experiment. As a reminder, the experiment is currently only open to those who meet the following criteria:</p>'+
    '<ul>'+
        '<li>You are 18 years of age or older</li>'+
        '<li>You are a native speaker of Korean or Mandarin</li>'+
        '<li>You started learning English at age 8 or older</li>'+
        '<li>You have not already participated in one of this researcher\'s studies</li>'+
    '</ul>'+
    '<p>If you believe that you have received this message in error, please contact me at fzenker@hawaii.edu.</p>'
;
var ko_language_survey_feedback_stimulus =
    '<style>p {text-align:left;} li {margin-bottom:10px} ul {text-align:left}</style>'+
    '<img src="img_warning.png" height="150" title="image credit: Flaticon / Freepik (www.freepik.com)"><br>'+
    '<h3 style="text-align:center">실험 조건 부적합 안내</h3>'+
    '<p>작성하여 주신 답변에 따르면 본 실험에 참가할 수 없습니다. 실험은 아래의 조건에 부합하는 사람만을 대상으로 하고 있습니다:</p>'+
    '<ul>'+
        '<li>만 18세 이상</li>'+
        '<li>한국어 원어민</li>'+
        '<li>영어를 만 8세 이후에 처음 공부하기 시작한 경우</li>'+
        '<li>본 연구자의 이전 실험에 참여한 적이 없는 분</li>'+
    '</ul>'+
    '<p>만약 위의 조건에 부합한다고 생각되신다면 연구자에게 fzenker@hawaii.edu로 메일을 보내주시기 바랍니다.</p>'
;
var zh_language_survey_feedback_stimulus =
    '<style>p {text-align:left;} li {margin-bottom:10px} ul {text-align:left}</style>'+
    '<img src="img_warning.png" height="150" title="image credit: Flaticon / Freepik (www.freepik.com)"><br>'+
    '<h3 style="text-align:center">注意：不符合參加資格</h3>'+
    '<p>根據您提供的回答，您可能不符合參加本次實驗的標準。本次實驗僅開放給符合以下標準者：</p>'+
    '<ul>'+
        '<li>18歲以上</li>'+
        '<li>中文母語者</li>'+
        '<li>8歲以後才開始學習英文</li>'+
        '<li>您沒有參加過本研究者(Fred Zenker)的其他實驗</li>'+
    '</ul>'+
    '<p>如果您相信您不該收到這份不符合資格的訊息，請您聯絡我 fzenker@hawaii.edu</p>'
;
var ko_language_survey_feedback_stimulus_lbc =
    '<style>p {text-align:left;} li {margin-bottom:10px} ul {text-align:left}</style>'+
    '<img src="img_warning.png" height="150" title="image credit: Flaticon / Freepik (www.freepik.com)"><br>'+
    '<h3 style="text-align:center">실험 조건 부적합 안내</h3>'+
    '<p>작성하여 주신 답변에 따르면 본 실험에 참가할 수 없습니다. 실험은 아래의 조건에 부합하는 사람만을 대상으로 하고 있습니다:</p>'+
    '<ul>'+
        '<li>만 18세 이상</li>'+
        '<li>한국어 원어민</li>'+
    '</ul>'+
    '<p>만약 위의 조건에 부합한다고 생각되신다면 연구자에게 fzenker@hawaii.edu로 메일을 보내주시기 바랍니다.</p>'
;

// *** elicited production task ***

var ept_instructions_header =
    '<style>p {text-align:left;} .section {padding:.5em;} table {border-collapse: collapse;} .row {border:3px solid #c6c6c6;box-shadow:2px 2px 5px #999; padding:.5em;} td {text-align:left;}'+inactive_btn+' .center {display: flex; justify-content: center; align-items: center;}</style>'+ 
    '<img src="img_ept.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)"><br>'
;

var en_ept_instructions_text =
    '<span style="font-size:125%; font-weight:bold;">Task 1: Sentence Recording Task</span>'+
    '<p style="text-align:center;">Check the box next to each section to indicate that you have read and understood the instructions.</p>'+
    '<table><tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>In this task, you will see pictures of people accompanied by sentences describing them. After reading the sentences, you will be asked a question about one of the people, and your response will be audio recorded.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>First, you will see a pair of pictures with sentences describing them, as shown below.</p>'+
    '<div style="width:500px; margin:auto; border:2px; border-style:ridge; padding:.5em;"><table style="margin-left:auto; margin-right:auto;"><tr><td><img src="img_woman1.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td>This woman won the race.</td></tr><tr><td><img src="img_woman2.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td>This woman lost the race.</td></tr></table><br><div class="center"><button class="inactive-btn">Continue</button></div></div>'+
    '<p>You should study the sentences carefully before continuing to the next page.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions3" value="agree" required></td>'+
    '<td><div class="section">'+
    'Next, one of the sentences will be replaced by a question. Your job is to answer the question as clearly and directly as possible.</p>'+
    '<div style="width:500px; margin:auto; border:2px; border-style:ridge; padding:.5em"><table style="margin-left:auto; margin-right:auto;"><tr><td><img src="img_woman1.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td><b><u>Which woman is this?</b></u></td></tr><tr><td><img src="img_woman2.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td>This woman lost the race.</td></tr></table><br><div class="center"><span style="color:red;"><b>Recording...</b></span></div></div>'+
    '<p>The \"Recording...\" message at the bottom of the screen lets you know that your voice is being recorded. You will have <b>six seconds</b> to give your response.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions4" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p><span style="background-color:lightgreen">\"the woman that won the race\"</span> and <span style="background-color:lightgreen">\"the one that won the race\"</span> are both <b>good answers</b> to the question above.</p>'+
    '<p><span style="background-color:lightpink">\"This woman won the race\"</span> is a <b>bad answer</b> because it simply repeats the original sentence and does not directly answer the question.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions5" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>After the recording is complete, you will have a chance to listen to your response and record it again if necessary.</p>'+
    '<div style="width:500px; margin:auto; border:2px; border-style:ridge; padding:.5em"><table style="margin-left:auto; margin-right:auto;"><tr><td><img src="img_woman1.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td><b><u>Which woman is this?</b></u></td></tr><tr><td><img src="img_woman2.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td>This woman lost the race.</td></tr></table><br><div class="center"><i>Press the play button to hear your response</i></div><br><div class="center"><img src="img_recorder.png" style="width:300px;"></img></div><br><div class="center"><button class="inactive-btn">Accept Recording</button>&nbsp;&nbsp;<button class="inactive-btn">Record Again</button></div></div>'+
    '<p>You should play back your responses for at least the first few trials to make sure that they are being recorded correctly.</p>'+
    '</div></td></tr>'+
    '</table>'+
    '<p style="text-align:center;">The task will take about 10&ndash;15 minutes, and during that time you should give it your full attention.</p><br>'
;
var ko_ept_instructions_text =
    '<span style="font-size:125%; font-weight:bold;">첫번째 과제: 영어 문장 녹음</span>'+
    '<p style="text-align:center;">실험에 대한 설명을 모두 읽고 이해하신다면 각 설명 앞에 위치한 박스를 클릭하여 주시기 바랍니다.</p>'+
    '<table><tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>본 과제에서는 두 명의 사람과 그들을 묘사하는 문장이 주어질 것입니다. 그 문장을 읽은 뒤, 둘 중 한 명에 대한 질문이 주어지고 그에 대한 답변을 녹음하게 됩니다.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>먼저, 아래와 같이 두 명의 사람과 그들을 묘사하는 문장이 주어질 것입니다.</p>'+
    '<div style="width:500px; margin:auto; border:2px; border-style:ridge; padding:.5em;"><table style="margin-left:auto; margin-right:auto;"><tr><td><img src="img_woman1.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td>This woman won the race.</td></tr><tr><td><img src="img_woman2.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td>This woman lost the race.</td></tr></table><br><div class="center"><button class="inactive-btn">계속</button></div></div>'+
    '<p>문장을 잘 읽고 다음 페이지로 이동하여 주십시오.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions3" value="agree" required></td>'+
    '<td><div class="section">'+
    '다음으로 하나의 문장이 질문으로 교체될 것입니다. 그 질문에 최대한 명확하게 답변하여 주시기 바랍니다.</p>'+
    '<div style="width:500px; margin:auto; border:2px; border-style:ridge; padding:.5em"><table style="margin-left:auto; margin-right:auto;"><tr><td><img src="img_woman1.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td><b><u>Which woman is this?</b></u></td></tr><tr><td><img src="img_woman2.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td>This woman lost the race.</td></tr></table><br><div class="center"><span style="color:red;"><b>Recording...</b></span></div></div>'+
    '<p>화면 하단의 \"Recording...\" 이라는 메세지는 목소리가 녹음되고 있다는 것을 알리는 것입니다. 녹음이 시작되면 <b>6초</b> 동안 답변을 녹음 하실 수 있습니다.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions4" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p><span style="background-color:lightgreen">\"the woman that won the race\"</span> 또는 <span style="background-color:lightgreen">\"the one that won the race\"</span> 라는 답변 모두 좋은 답변 입니다.</p>'+
    '<p><span style="background-color:lightpink">\"This woman won the race\"</span> 라는 답변은 주어진 문장을 단순히 반복하고 실제 질문에 대한 답변이 아니기 때문에 틀린 답변입니다.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions5" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>녹음이 끝난 뒤, 답변을 들어볼 수 있는 기회가 있으며, 필요하다면 다시 녹음하실 수 있습니다.</p>'+
    '<div style="width:500px; margin:auto; border:2px; border-style:ridge; padding:.5em"><table style="margin-left:auto; margin-right:auto;"><tr><td><img src="img_woman1.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td><b><u>Which woman is this?</b></u></td></tr><tr><td><img src="img_woman2.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td>This woman lost the race.</td></tr></table><br><div class="center"><i>재생 버튼을 누르면 답변을 들을 수 있습니다</i></div><br><div class="center"><img src="img_recorder.png" style="width:300px;"></img></div><br><div class="center"><button class="inactive-btn">녹음 저장</button>&nbsp;&nbsp;<button class="inactive-btn">다시 녹음</button></div></div>'+
    '<p>과제 초반에 답변들이 제대로 녹음되어 저장되고 있는지 확인차 들어주시면 감사하겠습니다.</p>'+
    '</div></td></tr>'+
    '</table>'+
    '<p style="text-align:center;">이 과제는 약 10&ndash;15분 정도 소요될 예정이며, 과제 수행 시 최대한 집중하여 주시기 바랍니다.</p><br>'
;
var zh_ept_instructions_text =
    '<span style="font-size:125%; font-weight:bold;">測驗1:句子錄音</span>'+
    '<p style="text-align:center;">請在框框中打勾確認您已閱讀並了解指示。</p>'+
    '<table><tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>在這個測驗中，您會看到有人的圖片和描述圖片的句子。在讀完句子後，請回答圖片中顯示的問題，您的回答會被錄音下來。</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>首先，您會看到一對圖片和描述它們的句子 （見以下）。</p>'+
    '<div style="width:500px; margin:auto; border:2px; border-style:ridge; padding:.5em;"><table style="margin-left:auto; margin-right:auto;"><tr><td><img src="img_woman1.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td>This woman won the race.</td></tr><tr><td><img src="img_woman2.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td>This woman lost the race.</td></tr></table><br><div class="center"><button class="inactive-btn">繼續</button></div></div>'+
    '<p>請仔細閱讀句子後，再前往下一頁。</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions3" value="agree" required></td>'+
    '<td><div class="section">'+
    '接下來，其中一句句子會變成問題。您的任務是要盡可能清楚且直接的回答問題。</p>'+
    '<div style="width:500px; margin:auto; border:2px; border-style:ridge; padding:.5em"><table style="margin-left:auto; margin-right:auto;"><tr><td><img src="img_woman1.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td><b><u>Which woman is this?</b></u></td></tr><tr><td><img src="img_woman2.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td>This woman lost the race.</td></tr></table><br><div class="center"><span style="color:red;"><b>Recording...</b></span></div></div>'+
    '<p>畫面下方的訊息\"Recording...\" 表示系統正在錄製您的聲音。您有<b>6秒</b>的時間可以回答問題。</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions4" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p><span style="background-color:lightgreen">\"the woman that won the race\"</span> 和 <span style="background-color:lightgreen">\"the one that won the race\"</span> 都是適<b>合</b>回答上方問題的<b>好</b>答案。</p>'+
    '<p><span style="background-color:lightpink">\"This woman won the race\"</span> <b>不適合</b>用來回答上方問題因為這個句子只有復述原本的句子而且沒有直接回答問題。</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions5" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>錄音結束後，您將有機會可以重聽您的回答並選擇重新錄音(如有需要)。</p>'+
    '<div style="width:500px; margin:auto; border:2px; border-style:ridge; padding:.5em"><table style="margin-left:auto; margin-right:auto;"><tr><td><img src="img_woman1.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td><b><u>Which woman is this?</b></u></td></tr><tr><td><img src="img_woman2.png" style="width:100px;" title="image credit: freepik / Freepik (www.freepik.com)"></img></td><td>This woman lost the race.</td></tr></table><br><div class="center"><i>按下播放按鈕以聆聽您的回答</i></div><br><div class="center"><img src="img_recorder.png" style="width:300px;"></img></div><br><div class="center"><button class="inactive-btn">接受錄音</button>&nbsp;&nbsp;<button class="inactive-btn">重新錄音</button></div></div>'+
    '<p>在一開始幾道題目，您應該播放並確認您的回答是否被正確錄製。</p>'+
    '</div></td></tr>'+
    '</table>'+
    '<p style="text-align:center;">這個測驗大概會花10&ndash;15分鐘的時間，在這段時間中請全神貫注在測驗上。</p><br>'
;

var en_ept_instructions_html =
    ept_instructions_header+en_ept_instructions_text
;
var ko_ept_instructions_html =
    ept_instructions_header+ko_ept_instructions_text
;
var zh_ept_instructions_html =
    ept_instructions_header+zh_ept_instructions_text
;

var ept_prac_alert_header =
    '<img src="img_practice.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)"><br>'
;

var en_ept_prac_alert_text =
    '<span style="font-size:125%; font-weight:bold;">Practice</span>'+
    '<p>Now we will do a few practice trials to familiarize you with the procedure.</p>'+
    '<p>For this practice phase, you will select the correct response instead of being recorded.</p>'
;
var ko_ept_prac_alert_text =
    '<span style="font-size:125%; font-weight:bold;">연습</span>'+
    '<p>지금부터 과제 수행 방식을 이해하기 위해 몇 개의 연습 문제를 진행하겠습니다.</p>'+
    '<p>여기서는 실제 답변을 녹음하는 것은 아니고 옳은 답변을 선택하는 연습을 하게 됩니다.</p>'
;
var zh_ept_prac_alert_text =
    '<span style="font-size:125%; font-weight:bold;">練習</span>'+
    '<p>現在我們將練習幾道題目來幫助您熟悉如何作答。</p>'+
    '<p>在練習階段，您只需要選擇正確的答案，不需要錄音。</p>'
;

var en_ept_prac_alert_stimulus =
    ept_prac_alert_header+en_ept_prac_alert_text
;
var ko_ept_prac_alert_stimulus =
    ept_prac_alert_header+ko_ept_prac_alert_text
;
var zh_ept_prac_alert_stimulus =
    ept_prac_alert_header+zh_ept_prac_alert_text
;

var start_task1_header = 
    '<img src="img_checked.png" height="150" title="image credit: Roundicons / Flaticon (www.flaticon.com)"><br>'
;

var en_start_task1_text =
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>Now you\'re ready to start the task.</p>'+
    '<p>From now on, your voice will be recorded when you are answering the questions.</p>'+
    '<p>Click the \'Continue\' button to enter fullscreen mode and begin.</p>'
;
var ko_start_task1_text =
    '<span style="font-size:125%; font-weight:bold;">수고하셨습니다!</span>'+
    '<p>이제 실제 과제로 넘어갈 것입니다.</p>'+
    '<p>실제 과제에서는 질문에 대한 답변을 녹음하게 될 것입니다.</p>'+
    '<p>\'계속\' 버튼을 눌러 시작하여 주십시오.</p>'
;
var zh_start_task1_text =
    '<span style="font-size:125%; font-weight:bold;">做得好！讚！</span>'+
    '<p>現在您將開始正式測驗。</p>'+
    '<p>從現在開始，您的聲音在作答時將會被錄製。</p>'+
    '<p>按下「繼續」按鈕來進入全螢幕並開始。</p>'
;

var en_start_task1_message =
    start_task1_header+en_start_task1_text
;
var ko_start_task1_message =
    start_task1_header+ko_start_task1_text
;
var zh_start_task1_message =
    start_task1_header+zh_start_task1_text
;

var en_ept_response_prompt =
    '<i>Press the play button to hear your response</i>'
;
var ko_ept_response_prompt =
    '<i>재생 버튼을 누르면 답변을 들을 수 있습니다</i>'
;
var zh_ept_response_prompt =
    '<i>按下播放按鈕以聆聽您的回答</i>'
;

var en_ept_response_button1 = 'Accept Recording';
var ko_ept_response_button1 = '녹음 저장';
var zh_ept_response_button1 = '接受錄音';

var en_ept_response_button2 = 'Record Again';
var ko_ept_response_button2 = '다시 녹음';
var zh_ept_response_button2 = '重新錄音';

// *** self-paced reading task ***

var spr_instructions_header =
    '<style>p {text-align:left;} .section {padding:.5em;} table {border-collapse: collapse;} .row {border:3px solid #c6c6c6;box-shadow:2px 2px 5px #999; padding:.5em;} td {text-align:left;}'+inactive_btn+' .center {display: flex; justify-content: center; align-items: center;}</style>'
;

var en_spr_instructions_text =
    '<img src="img_spr.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)"><br>'+
    '<span style="font-size:125%; font-weight:bold;">Task 2: Sentence Reading Task</span>'+
    '<p style="text-align:center;">Check the box next to each section to indicate that you have read and understood the instructions.</p>'+
    '<table><tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>In this task, you will read sentences and answer questions about them. Some of the sentences might sound better or worse to you than others, but try to focus on reading them for meaning.</p><br>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>You will need to press the <b>spacebar</b> to view the first word. Keep pressing the spacebar to move through the sentence one word at a time, as shown below.</p>'+
    '<div style="width:50%; margin:auto; border:2px; border-style:ridge; padding:1px;"><img src="img_segmentation.gif" style="display:block; margin-left:auto; margin-right: auto; width:16em;"></div>'+
    '<p>Try to press the spacebar fast enough to allow for relatively natural reading of the sentence. If you need to pause for a few seconds (to stretch, for example), do so between sentences and not in the middle of a sentence.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions3" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>After each sentence, there will be a short comprehension question like the one shown below.</p>'+
    '<div style="width:50%; margin:auto; border:2px; border-style:ridge; padding:2px;"><img src="img_question.gif" style="display:block; margin-left:auto; margin-right: auto; width:20em; padding:5px"></div>'+
    '<p>You will be notified if your answer is incorrect. When you get a question wrong, you should take it as a cue to read the sentences more carefully. Try to read the sentences as quickly as you can while still absorbing their meaning.</p>'+
    '</div></td></tr>'+
    '</table>'+
    '<p style="text-align:center;">The task will take about 10&ndash;15 minutes, and during that time you should give it your full attention.</p>'
;
var ko_spr_instructions_text =
    '<img src="img_spr.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)"><br>'+
    '<span style="font-size:125%; font-weight:bold;">두번째 과제: 영어 문장 읽기</span>'+
    '<p style="text-align:center;">실험에 대한 설명을 모두 읽고 이해하신다면 각 설명 앞에 위치한 박스를 클릭하여 주시기 바랍니다.</p>'+
    '<table><tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>본 과제에서는 주어진 문장을 읽고 질문에 답할 것입니다. 몇몇 문장은 다른 문장들보다 더 낫거나 어색하게 느껴질 수 있지만, 문장의 의미에 집중하여 주시길 부탁드립니다.</p><br>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>단어를 보기 위해서는 키보드의 <b>스페이스바를</b> 눌러주시기 바랍니다. 아래에 보이는 것과 같이, 스페이스바를 누르면 한 단어씩 나타나서 문장을 볼 수 있습니다.</p>'+
    '<div style="width:50%; margin:auto; border:2px; border-style:ridge; padding:1px;"><img src="img_segmentation.gif" style="display:block; margin-left:auto; margin-right: auto; width:16em;"></div>'+
    '<p>평소 자연스럽게 글을 읽는 속도로 문장이 보여지도록 속도에 맞추어 스페이스바를 눌러 주십시오. 만약 중간에 몇 초 정도 멈출 일이 생긴다면 (예를 들어 스트레칭을 하기 위해), 문장의 중간이 아닌, 한 문장이 끝난 후 다음 문장으로 진행하기 전에 멈추시길 바랍니다. </p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions3" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>각 문장이 끝나면, 아래에 보이는 것과 같이 객관식 문제가 나타날 것입니다.</p>'+
    '<div style="width:50%; margin:auto; border:2px; border-style:ridge; padding:2px;"><img src="img_question-korean.gif" style="display:block; margin-left:auto; margin-right: auto; width:20em; padding:5px"></div>'+
    '<p>만약 선택하신 답변이 틀렸다면 오답이라는 안내를 받게 됩니다. 답이 틀린 경우, 문장들을 더욱 주의해서 읽어주시기 바랍니다. 문장의 의미를 이해할 수 있는 한에서 최대한 빠르게 문장을 읽어주시기 바랍니다.</p>'+
    '</div></td></tr>'+
    '</table>'+
    '<p style="text-align:center;">이 과제는 약 10&ndash;15분 정도 소요될 예정이며, 과제 수행 시 최대한 집중하여 주시기 바랍니다.</p>'
;
var zh_spr_instructions_text =
    '<img src="img_spr.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)"><br>'+
    '<span style="font-size:125%; font-weight:bold;">測驗2:句子判讀測驗</span>'+
    '<p style="text-align:center;">請在框框中打勾確認您已閱讀並了解指示。</p>'+
    '<table><tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>在這個測驗中，您會閱讀句子並回答相關問題。有些句子會聽起來比其他要好或差，但請盡量專注在他們的意義而不是合不合文法。</p><br>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>您將需要按下<b>空白鍵</b>來開始第一個字。每按一次空白鍵，句子將會以一個字一個字的方式出現，如下所示。</p>'+
    '<div style="width:50%; margin:auto; border:2px; border-style:ridge; padding:1px;"><img src="img_segmentation.gif" style="display:block; margin-left:auto; margin-right: auto; width:16em;"></div>'+
    '<p>按空白鍵時，請盡量將速度控制在接近自然閱讀的狀況。如果您需要暫停一下（比如想閉目養神一下），您可以在句子和句子中間這麼做，但請不要在字和字的句子中間這麼做。 </p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions3" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>在每一句之後會有一個簡短的理解型問題（如下所示）。</p>'+
    '<div style="width:50%; margin:auto; border:2px; border-style:ridge; padding:2px;"><img src="img_question-mandarin.gif" style="display:block; margin-left:auto; margin-right: auto; width:28em; padding:5px"></div>'+
    '<p>如果您回答錯誤，系統將會顯示錯誤訊息。當看到錯誤訊息，請加倍仔細閱讀句子以正確回答問題。理想的閱讀速度是接近自然閱讀的狀況，所以請快速連續按下空白鍵，但又能理解句子的意義。</p>'+
    '</div></td></tr>'+
    '</table>'+
    '<p style="text-align:center;">這個測驗大概會花10&ndash;15分鐘的時間，在這段時間中請全神貫注在測驗上。</p>'
;

var en_spr_instructions_html = 
    spr_instructions_header+en_spr_instructions_text
;
var ko_spr_instructions_html = 
    spr_instructions_header+ko_spr_instructions_text
;
var zh_spr_instructions_html = 
    spr_instructions_header+zh_spr_instructions_text
;

var spr_prac_alert_header =
    '<img src="img_practice.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)"><br>'
;

var en_spr_prac_alert_text =
    '<span style="font-size:125%; font-weight:bold;">Practice</span>'+
    '<p>Now we will do a few practice trials to familiarize you with the procedure.</p>'
;
var ko_spr_prac_alert_text =
    '<span style="font-size:125%; font-weight:bold;">연습</span>'+
    '<p>지금부터 과제 수행 방식을 이해하기 위해 몇 개의 연습 문제를 진행하겠습니다.</p>'
;
var zh_spr_prac_alert_text =
    '<span style="font-size:125%; font-weight:bold;">練習</span>'+
    '<p>現在我們將練習幾道題目來幫助您熟悉如何作答。</p>'
;

var en_spr_prac_alert_stimulus =
    spr_prac_alert_header+en_spr_prac_alert_text
;
var ko_spr_prac_alert_stimulus =
    spr_prac_alert_header+ko_spr_prac_alert_text
;
var zh_spr_prac_alert_stimulus =
    spr_prac_alert_header+zh_spr_prac_alert_text
;

var start_task2_header =
    '<img src="img_checked.png" height="150" title="image credit: Roundicons / Flaticon (www.flaticon.com)"><br>'
;

var en_start_task2_text =
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>Now you\'re ready to start the task.</p>'+
    '<p>Click the \'Continue\' button to enter fullscreen mode and begin.</p>'
;
var ko_start_task2_text =
    '<span style="font-size:125%; font-weight:bold;">수고하셨습니다!</span>'+
    '<p>이제 실제 과제로 넘어갈 것입니다.</p>'+
    '<p>\'계속\' 버튼을 눌러 시작하여 주십시오.</p>'
;
var zh_start_task2_text =
    '<span style="font-size:125%; font-weight:bold;">做得好！讚！</span>'+
    '<p>現在您將開始正式測驗。</p>'+
    '<p>按下「繼續」按鈕來進入全螢幕並開始。</p>'
;

var en_start_task2_message =
    start_task2_header+en_start_task2_text
;
var ko_start_task2_message =
    start_task2_header+ko_start_task2_text
;
var zh_start_task2_message =
    start_task2_header+zh_start_task2_text
;

var en_spr_question_prompt =
    'What did we find out from the sentence?'
;
var ko_spr_question_prompt =
    '문장에서 알 수 있는 것은 무엇입니까?'
;
var zh_spr_question_prompt =
    '我們從句子中知道了什麼？'
;

// *** acceptability judgment task ***

var ajt_instructions_header =
    '<style>p {text-align:left;} .section {padding:.5em;} table {border-collapse: collapse;} .row {border:3px solid #c6c6c6;box-shadow:2px 2px 5px #999; padding:.5em;} td {text-align:left;}'+inactive_btn+' .center {display: flex; justify-content: center; align-items: center;}</style>'+
    '<img src="img_ajt.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)"><br>'
;

var en_ajt_instructions_text =
    '<span style="font-size:125%; font-weight:bold;">Task 3: Sentence Rating Task</span>'+
    '<p style="text-align:center;">Check the box next to each section to indicate that you have read and understood the instructions.</p>'+
    '<table><tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>Speakers of a language have an intuitive sense of which sentences are acceptable or unacceptable, even when they don\'t know exactly why. For example, most English speakers feel that the first sentence below is acceptable and that the second one is unacceptable.</p>'+
    '<p>Acceptable Sentence: <span style="background-color:lightgreen">\"Mary thinks that John is likely to pass the test even if he doesn\'t study this weekend.\"</span></p>'+
    '<p>Unacceptable Sentence: <span style="background-color:lightpink">\"Mary thinks that John is probable to pass the test even if he doesn\'t study this weekend.\"</span></p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>In this task, you will read sentences and rate them using the following scale where 1 means \"completely unacceptable\" and 6 means \"completely acceptable\":</p>'+
    '<div style="width:85%; margin:auto; border:2px; border-style:ridge; padding:2px;"><p style="text-align:center; font-size:1.1em;">I think John is likely to pass the test even if he doesn\'t study this weekend.</p><p style="text-align:center;"><i>Completely Unacceptable</i>&nbsp;&nbsp;<button class="inactive-btn">1</button>&nbsp;&nbsp;<button class="inactive-btn">2</button>&nbsp;&nbsp;<button class="inactive-btn">3</button>&nbsp;&nbsp;<button class="inactive-btn">4</button>&nbsp;&nbsp;<button class="inactive-btn">5</button>&nbsp;&nbsp;<button class="inactive-btn">6</button>&nbsp;&nbsp;<i>Completely Acceptable</i></p><p style="text-align:center;">Press the zero key if you cannot rate the sentence</p></div>'+
    '<p>When rating the sentences, ask yourself, \"Does this sound like something that might have been said on purpose by a native speaker of the language?\"</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions3" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>There is a chance you might occasionally encounter sentences that you are unable to rate. For example, you may be unable to rate a sentence if it contains words that you do not know. If this happens, you can press the zero (\"0\") key on your keyboard instead of providing a rating for that sentence. However, we expect that most participants will have little or no need for the zero option.</p>'+
    '</div></td></tr>'+
    '</table>'+
    '<p style="text-align:center;">The task will take about 10&ndash;15 minutes, and during that time you should give it your full attention.</p>'
;
var en_ajt_instructions_english_title =
    '<span style="font-size:125%; font-weight:bold;">Task 3: Rating Sentences in English</span>'
;
var ko_ajt_instructions_english_title =
    '<span style="font-size:125%; font-weight:bold;">세번째 과제: 영어 문장 평가</span>'
;
var zh_ajt_instructions_english_title =
    '<span style="font-size:125%; font-weight:bold;">測驗3：英文句子評分</span>'
;
var en_ajt_instructions_korean_title =
    '<span style="font-size:125%; font-weight:bold;">Task 4: Rating Sentences in Korean</span>'
;
var ko_ajt_instructions_korean_title =
    '<span style="font-size:125%; font-weight:bold;">네번째 과제: 한국어 문장 평가</span>'
;
var zh_ajt_instructions_korean_title =
    '<span style="font-size:125%; font-weight:bold;">測驗4：中文句子評分</span>'
;
var en_ajt_instructions_block1 =
    '<p style="text-align:center;">Check the box next to each section to indicate that you have read and understood the instructions.</p>'+
    '<table><tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>Speakers of a language have an intuitive sense of which sentences are acceptable or unacceptable, even when they don\'t know exactly why. For example, in Korean you may feel that sentences (1)&ndash;(3) below sounds like acceptable Korean sentences, while sentence (4) does not.</p>'+
    '<table style="margin: auto;"><tr><td style="padding: 10px">'+
    '(1) 영희의 눈이 크다. '+
    '<img src="img_checked.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td><td style="padding: 10px">'+
    '(2) 영희의 책이 크다. '+
    '<img src="img_checked.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td><tr><tr><td style="padding: 10px">'+
    '(3) 영희는 눈이 크다. '+
    '<img src="img_checked.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td><td style="padding: 10px">'+
    '(4) 영희는 책이 크다. '+
    '<img src="img_cancel.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td></tr></table>'+
    '<p>Although sentences (3) and (4) have the same structure, one can judge without depending on any rule that sentence (4) is not acceptable in Korean.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'
;
var ko_ajt_instructions_block1 =
    '<p style="text-align:center;">실험에 대한 설명을 모두 읽고 이해하신다면 각 설명 앞에 위치한 박스를 클릭하여 주시기 바랍니다.</p>'+
    '<table><tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>언어 사용자는 정확하게 이유를 설명할 수 없어도 어느 문장이 해당 언어에서 자연스러운 문장인지 아닌지를 아는 직관적인 감각을 갖고 있습니다. 예를 들어, 다음 한국어 문장 중 (1)&ndash;(3)번 문장은 자연스러운 문장으로 받아들일 수 있지만, (4)번 문장은 자연스러운 문장으로 받아들일 수 없다고 느낄 수 있습니다.</p>'+
    '<table style="margin: auto;"><tr><td style="padding: 10px">'+
    '(1) 영희의 눈이 크다. '+
    '<img src="img_checked.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td><td style="padding: 10px">'+
    '(2) 영희의 책이 크다. '+
    '<img src="img_checked.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td><tr><tr><td style="padding: 10px">'+
    '(3) 영희는 눈이 크다. '+
    '<img src="img_checked.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td><td style="padding: 10px">'+
    '(4) 영희는 책이 크다. '+
    '<img src="img_cancel.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td></tr></table>'+
    '<p>(3)번과 (4)번 문장이 같은 구조로 이루어져 있지만, 문법적인 근거를 고려하지 않고도 (4)번 문장은 자연스럽지 않다는 것을 판단할 수 있습니다.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'
;
var zh_ajt_instructions_block1 =
    '<p style="text-align:center;">請在框框中打勾確認您已閱讀並了解指示。</p>'+
    '<table><tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions1" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>一個語言的母語者擁有判斷一個句子能不能被接受的直覺，就算他們不一定知道接不接受背後的原因。舉例來說，您可能會覺得句子（1）在中文是可以接受的句子，但句子（2）不可以。</p>'+
    '<table style="margin: auto;"><tr><td style="padding: 10px">'+
    '(1) 小美明天很有可能會贏比賽。 '+
    '<img src="img_checked.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td><td style="padding: 10px">'+
    '(2) 小美贏比賽很有可能明天。 '+
    '<img src="img_cancel.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td></tr></table>'+
    '<p>您可以在不想到任何規則的狀況下，直接判斷（2）在中文中是不能被接受的句子。</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'
;
var en_ajt_instructions_english_example =
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>Likewise, in English you may feel that the first sentence below sounds like an acceptable sentence while the second one does not.</p>'+
    '<table style="margin: auto;"><tr><td style="padding: 10px">'+
    '(1) John is likely to win the race. '+
    '<img src="img_checked.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td><td style="padding: 10px">'+
    '(2) John is probable to win the race. '+
    '<img src="img_cancel.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td></tr></table>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'
;
var ko_ajt_instructions_english_example =
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>마찬가지로, 영어에서도 첫번째 문장은 자연스러운 문장으로 받아들일 수 있지만 두번째 문장은 자연스러운 문장으로 받아들일 수 없다고 느낄 수 있습니다.</p>'+
    '<table style="margin: auto;"><tr><td style="padding: 10px">'+
    '(1) John is likely to win the race. '+
    '<img src="img_checked.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td><td style="padding: 10px">'+
    '(2) John is probable to win the race. '+
    '<img src="img_cancel.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td></tr></table>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'
;
var zh_ajt_instructions_english_example =
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>同樣的，您可能也可以感受到下面第一個句子像是一個可以被接受的句子，但第二個就不行。</p>'+
    '<table style="margin: auto;"><tr><td style="padding: 10px">'+
    '(1) John is likely to win the race. '+
    '<img src="img_checked.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td><td style="padding: 10px">'+
    '(2) John is probable to win the race. '+
    '<img src="img_cancel.png" height="20" title="image credit: vectorjuice / Freepik (www.freepik.com)" style="display: inline-block;">'+
    '</td></tr></table>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'
;
var en_ajt_instructions_block2 =
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>In this task, you will read sentences and rate them using the following scale where 1 means \"completely unacceptable\" and 6 means \"completely acceptable\":</p>'+
    '<div style="width:85%; margin:auto; border:2px; border-style:ridge; padding:2px;"><p style="text-align:center; font-size:1.1em;">'
;
var ko_ajt_instructions_block2 =
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>본 과제에서는 주어진 문장을 읽고 문장이 자연스러운지를 6개의 척도를 사용하여 평가할 것입니다. 해당 문장이 매우 부자연스럽다면 1을, 매우 자연스럽다면 6을 눌러주세요.</p>'+
    '<div style="width:85%; margin:auto; border:2px; border-style:ridge; padding:2px;"><p style="text-align:center; font-size:1.1em;">'
;
var zh_ajt_instructions_block2 =
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions2" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>在這個測驗中，您將會閱讀句子並給他們評分。請使用這個評分標準：1分代表「完全不能接受」而6分代表「完全可以接受」:</p>'+
    '<div style="width:85%; margin:auto; border:2px; border-style:ridge; padding:2px;"><p style="text-align:center; font-size:1.1em;">'
;
var ko_ajt_instructions_english_stimulus =
    'John is likely to win the race.'
;
var zh_ajt_instructions_english_stimulus =
    'John is likely to win the race.'
;
var ko_ajt_instructions_korean_stimulus =
    '영희의 책이 크다.'
;
var zh_ajt_instructions_mandarin_stimulus =
    '小美明天很有可能會贏比賽。'
;
var en_ajt_instructions_block3 =  
    '</p><p style="text-align:center;"><i>Completely Unacceptable</i>&nbsp;&nbsp;<button class="inactive-btn">1</button>&nbsp;&nbsp;<button class="inactive-btn">2</button>&nbsp;&nbsp;<button class="inactive-btn">3</button>&nbsp;&nbsp;<button class="inactive-btn">4</button>&nbsp;&nbsp;<button class="inactive-btn">5</button>&nbsp;&nbsp;<button class="inactive-btn">6</button>&nbsp;&nbsp;<i>Completely Acceptable</i></p><p style="text-align:center;">Press the zero key if you cannot rate the sentence</p></div>'+
    '<p>We are interested in your intuitions about the sentences. When rating the sentences, ask yourself, \"Does this sound like something that might have been said on purpose by a native speaker of the language?\"</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions3" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>There is a chance you might occasionally encounter sentences that you are unable to rate. For example, you may be unable to rate a sentence if it contains words that you do not know. If this happens, you can press the zero (\"0\") key on your keyboard instead of providing a rating for that sentence. However, we expect that most participants will have little or no need for the zero option.</p>'+
    '</div></td></tr>'+
    '</table>'+
    '<p style="text-align:center;">The task will take about 10&ndash;15 minutes, and during that time you should give it your full attention.</p>'
;
var ko_ajt_instructions_block3 =  
    '</p><p style="text-align:center;"><i>매우 부자연스러움</i>&nbsp;&nbsp;<button class="inactive-btn">1</button>&nbsp;&nbsp;<button class="inactive-btn">2</button>&nbsp;&nbsp;<button class="inactive-btn">3</button>&nbsp;&nbsp;<button class="inactive-btn">4</button>&nbsp;&nbsp;<button class="inactive-btn">5</button>&nbsp;&nbsp;<button class="inactive-btn">6</button>&nbsp;&nbsp;<i>매우 자연스러움</i></p><p style="text-align:center;">만약 문장의 자연스러움을 평가할 수 없다면 키보드의 0을 눌러주십시오.</p></div>'+
    '<p>본 실험은 문장을 판단하는 당신의 직관력에 관심을 가지고 있습니다. 문장을 평가할 때, \"이 문장이 원어민이 실제 의도적으로 사용할 것 같은 문장인지\" 고려하여 주시기 바랍니다.</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions3" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>문장의 자연스러움을 평가할 수 없는 경우가 생길 수 있습니다. 예를 들어, 모르는 단어가 포함되어 있다면 문장의 자연스러움을 평가하기 어려울 수 있습니다. 그런 경우, 문장을 평가하지 않고 키보드의 숫자 0을 눌러주십시오. 하지만 대부분의 참가자들에게 있어, 그런 경우는 없거나 매우 적을 것입니다.</p>'+
    '</div></td></tr>'+
    '</table>'+
    '<p style="text-align:center;">이 과제는 약 10&ndash;15분 정도 소요될 예정이며, 과제 수행 시 최대한 집중하여 주시기 바랍니다.</p>'
;
var zh_ajt_instructions_block3 =  
    '</p><p style="text-align:center;"><i>完全不能接受</i>&nbsp;&nbsp;<button class="inactive-btn">1</button>&nbsp;&nbsp;<button class="inactive-btn">2</button>&nbsp;&nbsp;<button class="inactive-btn">3</button>&nbsp;&nbsp;<button class="inactive-btn">4</button>&nbsp;&nbsp;<button class="inactive-btn">5</button>&nbsp;&nbsp;<button class="inactive-btn">6</button>&nbsp;&nbsp;<i>完全可以接受</i></p><p style="text-align:center;">如果您難以給出分數，請按0</p></div>'+
    '<p>我們感興趣的是您對於句子的直覺。當給句子評分的時候，請問問自己：「這聽起來像是母語者會說的話嗎？」</p>'+
    '</div></td></tr>'+
    '<tr class="spacer" height="20"><td></td></tr>'+
    '<tr class="row"><td style="background-color:#c6c6c6;"><input type="checkbox" name="instructions3" value="agree" required></td>'+
    '<td><div class="section">'+
    '<p>另外，有時候您很有可能會無法給出評分，像是您不知道有些字的意思。這種時候請您直接按數字鍵0就可以了，請不要提供評分。話雖如此，我們預期大多數的受試者並不需要使用0。</p>'+
    '</div></td></tr>'+
    '</table>'+
    '<p style="text-align:center;">這個測驗大概會花10&ndash;15分鐘的時間，在這段時間中請全神貫注在測驗上。</p>'
;

var en_ajt_instructions_html =
    ajt_instructions_header + en_ajt_instructions_text
;
var ko_ajt_instructions_html =
    ajt_instructions_header + 
    ko_ajt_instructions_english_title +
    ko_ajt_instructions_block1 +
    ko_ajt_instructions_english_example +
    ko_ajt_instructions_block2 +
    ko_ajt_instructions_english_stimulus +
    ko_ajt_instructions_block3
;
var zh_ajt_instructions_html =
    ajt_instructions_header + 
    zh_ajt_instructions_english_title +
    zh_ajt_instructions_block1 +
    zh_ajt_instructions_english_example +
    zh_ajt_instructions_block2 +
    zh_ajt_instructions_english_stimulus +
    zh_ajt_instructions_block3
;
var ko_ajt2_instructions_html =
    ajt_instructions_header + 
    ko_ajt_instructions_korean_title +
    ko_ajt_instructions_block1 +
    ko_ajt_instructions_block2 +
    ko_ajt_instructions_korean_stimulus +
    ko_ajt_instructions_block3
;
var zh_ajt2_instructions_html =
    ajt_instructions_header + 
    zh_ajt_instructions_korean_title +
    zh_ajt_instructions_block1 +
    zh_ajt_instructions_block2 +
    zh_ajt_instructions_mandarin_stimulus +
    zh_ajt_instructions_block3
;

var ajt_practice_alert_header = 
    '<img src="img_practice.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)"><br>'
;

var en_ajt_practice_alert_text =
    '<span style="font-size:125%; font-weight:bold;">Practice</span><br>'+
    '<p>Now we will do a few practice trials to familiarize you with the procedure.</p>'+
    '<div style="width:90%; margin:auto; border:2px; border-style:ridge; padding-left:1em; padding-right:1em; text-align:left;">'+
    '<p>A few things to keep in mind during the practice phase:</p>'+
    '<ul style="margin-top:-15px;">'+
    '<li>Each of the practice trials is designed to have either high or low acceptability.</li>'+
    '<li>You should give high ratings to the acceptable sentences and low ratings to the unacceptable ones.</li>'+
    '<li>You will be notified if your answer is incorrect.</li>'+
    '</div><br>'
;
var ko_ajt_practice_alert_text =
    '<span style="font-size:125%; font-weight:bold;">연습</span><br>'+
    '<p>지금부터 과제 수행 방식을 이해하기 위해 몇 개의 연습 문제를 진행하겠습니다.</p>'+
    '<div style="width:90%; margin:auto; border:2px; border-style:ridge; padding-left:1em; padding-right:1em; text-align:left;">'+
    '<p>몇 가지 주의사항이 있습니다:</p>'+
    '<ul style="margin-top:-15px;">'+
    '<li>각 문장은 자연스럽거나 부자연스럽게 만들어져 있습니다.</li>'+
    '<li>각 문장을 평가할 때 수용성이 높은 경우 (문장이 자연스러운 경우) 높은 점수를, 수용성이 낮은 경우 (문장이 부자연스러운 경우) 낮은 점수를 주시기 바랍니다.</li>'+
    '<li>만약 선택하신 답변이 틀렸다면 오답이라는 안내를 받게 됩니다.</li>'+
    '</div><br>'
;
var zh_ajt_practice_alert_text =
    '<span style="font-size:125%; font-weight:bold;">練習</span><br>'+
    '<p>現在我們將練習幾道題目來幫助您熟悉如何作答。</p>'+
    '<div style="width:90%; margin:auto; border:2px; border-style:ridge; padding-left:1em; padding-right:1em; text-align:left;">'+
    '<p>做練習題時，有幾件事情要麻煩您留心：</p>'+
    '<ul style="margin-top:-15px;">'+
    '<li>在這些題目中，有些句子讀起來應該是很可以接受的，但也有些句子是刻意被設計成不太能接受的</li>'+
    '<li>在答題時，請您給很可以接受的句子高分，無法接受的句子低分</li>'+
    '<li>如果您的回答不正確，系統將會提醒您</li>'+
    '</div><br>'
;

var en_ajt_practice_alert_stimulus =
    ajt_practice_alert_header+en_ajt_practice_alert_text
;
var ko_ajt_practice_alert_stimulus =
    ajt_practice_alert_header+ko_ajt_practice_alert_text
;
var zh_ajt_practice_alert_stimulus =
    ajt_practice_alert_header+zh_ajt_practice_alert_text
;

var start_ajt_header =
    '<img src="img_checked.png" height="150" title="image credit: Roundicons / Flaticon (www.flaticon.com)"><br>'
;

var en_start_ajt_text =
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>Now you\'re ready to start the task.</p>'+
    '<p>You will no longer be notified when your answers are incorrect.</p>'+
    '<p>Click the \'Continue\' button to enter fullscreen mode and begin.</p>'
;
var ko_start_ajt_text =
    '<span style="font-size:125%; font-weight:bold;">수고하셨습니다!</span>'+
    '<p>이제 실제 과제로 넘어갈 것입니다.</p>'+
    '<p>실제 과제에서는 정답여부는 나타나지 않습니다.</p>'+
    '<p>\'계속\' 버튼을 눌러 전체화면으로 이동 후 시작하여 주십시오.</p>'
;
var zh_start_ajt_text =
    '<span style="font-size:125%; font-weight:bold;">做得好！讚！</span>'+
    '<p>現在您將開始正式測驗。</p>'+
    '<p>如果您的回答不正確，現在開始系統將不再提醒您。</p>'+
    '<p>按下「繼續」按鈕來進入全螢幕並開始。</p>'
;

var en_start_ajt_message =
    start_ajt_header+en_start_ajt_text
;
var ko_start_ajt_message =
    start_ajt_header+ko_start_ajt_text
;
var zh_start_ajt_message =
    start_ajt_header+zh_start_ajt_text
;

var en_leftComment =
    '<i>Completely Unacceptable</i>'
;
var ko_leftComment =
    '<i>매우 부자연스러움</i>'
;
var zh_leftComment =
    '<i>完全不能接受</i>'
;

var en_rightComment =
    '<i>Completely Acceptable</i>'
;
var ko_rightComment =
    '<i>매우 자연스러움</i>'
;
var zh_rightComment =
    '<i>完全可以接受</i>'
;

var en_zero_option =
    '<p>Press the zero key if you cannot rate the sentence</p>'
;
var ko_zero_option =
    '<p>만약 문장의 자연스러움을 평가할 수 없다면 키보드의 0을 눌러주십시오.</p>'
;
var zh_zero_option =
    '<p>如果您難以給出分數，請按0</p>'
;

// *** ctest ***

var ctest_header =
    '<style>p {text-align:left; spellcheck=false;} input[type="text"] {width:8ch;} fieldset {border:1px solid #999;box-shadow:2px 2px 5px #999;} legend {background:#fff;text-align:left;font-size:110%;}</style>'+
    '<img src="img_ctest.jpg" height="150" title="image credit: pch.vector / Freepik (www.freepik.com)"><br>'
;

var en_ctest_instructions =
    '<span style="font-size:125%; font-weight:bold;">Task 4: Gap-Filling Task</span>'+
    '<p><b>Instructions:</b> In the following passages, parts of some words have been deleted. Your job is to complete the words by filling in the missing letters. In cases where more than one answer is possible, choose the one that fits best in the context. If you do not know the right answer, then type in your best guess.</p>'+
    '<p><b>Example:</b> The world is full of flowers. They <nobr>co<input type="text" name="example1" value="me" readonly="readonly"></nobr> in <nobr>diff<input type="text" name="example2" value="erent" readonly="readonly"></nobr> shapes <nobr>a<input type="text" name="example3" value="nd" readonly="readonly"></nobr> colors.</p>'
;
var ko_ctest_instructions =
    '<span style="font-size:125%; font-weight:bold;">마지막 과제: 영어 빈칸 채우기</span>'+
    '<p><b>방법:</b> 아래 문단의 몇몇 단어들은 부분적으로 빈칸이 남겨져있습니다. 그 누락된 글자들을 채워 단어를 완성하는 과제입니다. 만약 답을 모르는 경우, 가장 근접한 단어로 추측하여 주십시오.</p>'+
    '<p><b>예시:</b> The world is full of flowers. They <nobr>co<input type="text" name="example1" value="me" readonly="readonly"></nobr> in <nobr>diff<input type="text" name="example2" value="erent" readonly="readonly"></nobr> shapes <nobr>a<input type="text" name="example3" value="nd" readonly="readonly"></nobr> colors.</p>'
;
var zh_ctest_instructions =
    '<span style="font-size:125%; font-weight:bold;">最終測驗：英文填空題</span>'+
    '<p><b>指示：</b>在下面的句子中，您會發現我們拿掉了某些字的一些字母。您的工作是把這些缺失的字母補齊。在有多重答案的空格中，請依據前後文選出最適合的答案。如果您不知道正確答案，請試著猜猜看可能的答案。</p>'+
    '<p><b>例如：</b> The world is full of flowers. They <nobr>co<input type="text" name="example1" value="me" readonly="readonly"></nobr> in <nobr>diff<input type="text" name="example2" value="erent" readonly="readonly"></nobr> shapes <nobr>a<input type="text" name="example3" value="nd" readonly="readonly"></nobr> colors.</p>'
;

var ctest_passages =
    '<fieldset><legend style="text-align:left;"><b>Passage 1</b></legend>'+
    '<p spellcheck="false">Algae are organisms, or living things, that are found all over the world. Algae are <nobr>ve<input name="algae_01" type="text" required></nobr> important <nobr>bec<input name="algae_02" type="text" required></nobr> they <nobr>ma<input name="algae_03" type="text" required></nobr> oxygen, <nobr>wh<input name="algae_04" type="text" required></nobr> humans and other animals <nobr>ne<input name="algae_05" type="text" required></nobr> to <nobr>bre<input name="algae_06" type="text" required>.</nobr> Some algae, such as <nobr>sea<input name="algae_07" type="text" required>,</nobr> look <nobr>li<input name="algae_08" type="text" required></nobr> plants. <nobr>How<input name="algae_09" type="text" required>,</nobr> algae are <nobr>actu<input name="algae_10" type="text" required></nobr> neither plants <nobr>n<input name="algae_11" type="text" required></nobr> animals. <nobr>Ins<input name="algae_12" type="text" required>,</nobr> they <nobr>bel<input name="algae_13" type="text" required></nobr> to a <nobr>gr<input name="algae_14" type="text" required></nobr> of living things <nobr>cal<input name="algae_15" type="text" required></nobr> protists. <nobr>Th<input name="algae_16" type="text" required></nobr> are <nobr>ma<input name="algae_17" type="text" required></nobr> different <nobr>spe<input name="algae_18" type="text" required>,</nobr> or <nobr>ty<input name="algae_19" type="text" required>,</nobr> of algae. They are <nobr>mo<input name="algae_20" type="text" required></nobr> commonly found in <nobr>bod<input name="algae_21" type="text" required></nobr> of <nobr>wa<input name="algae_22" type="text" required>,</nobr> such as <nobr>oce<input name="algae_23" type="text" required>,</nobr> rivers, <nobr>la<input name="algae_24" type="text" required>,</nobr> streams, <nobr>po<input name="algae_25" type="text" required>,</nobr> and marshes.</p></fieldset><br>'+
    '<fieldset><legend style="text-align:left;"><b>Passage 2</b></legend>'+
    '<p spellcheck="false">Noise pollution is unwanted or excessive sound that can be harmful to human health and environmental quality. Noise pollution is commonly <nobr>gene<input name="noise_26" type="text" required></nobr> inside <nobr>indus<input name="noise_27" type="text" required></nobr> facilities and other <nobr>workp<input name="noise_28" type="text" required>,</nobr> but it also <nobr>co<input name="noise_29" type="text" required></nobr> from <nobr>hig<input name="noise_30" type="text" required>,</nobr> railway, and <nobr>airp<input name="noise_31" type="text" required></nobr> traffic and from outdoor <nobr>constr<input name="noise_32" type="text" required></nobr> activities. Noise is <nobr>mo<input name="noise_33" type="text" required></nobr> than a <nobr>me<input name="noise_34" type="text" required></nobr> nuisance. At <nobr>cer<input name="noise_35" type="text" required></nobr> levels and <nobr>dura<input name="noise_36" type="text" required></nobr> of exposure, it can cause <nobr>phys<input name="noise_37" type="text" required></nobr> damage to the <nobr>ear<input name="noise_38" type="text" required></nobr> and the <nobr>sens<input name="noise_39" type="text" required></nobr> hairs of the <nobr>in<input name="noise_40" type="text" required></nobr> ear and <nobr>res<input name="noise_41" type="text" required></nobr> in <nobr>temp<input name="noise_42" type="text" required></nobr> or <nobr>perm<input name="noise_43" type="text" required></nobr> hearing loss. In <nobr>addi<input name="noise_44" type="text" required></nobr> to causing hearing loss, excessive noise exposure can also <nobr>ra<input name="noise_45" type="text" required></nobr> blood <nobr>pres<input name="noise_46" type="text" required>,</nobr> cause irritability, <nobr>anx<input name="noise_47" type="text" required>,</nobr> and <nobr>men<input name="noise_48" type="text" required></nobr> fatigue, and <nobr>inte<input name="noise_49" type="text" required></nobr> with <nobr>sl<input name="noise_50" type="text" required>,</nobr> recreation, and personal communication.</p></fieldset><br>'
;

var en_ctest_html =
    ctest_header+en_ctest_instructions+ctest_passages
;
var ko_ctest_html =
    ctest_header+ko_ctest_instructions+ctest_passages
;
var zh_ctest_html =
    ctest_header+zh_ctest_instructions+ctest_passages
;

// *** breaks and submission pages ***

var break_header =
    '<style>.code {font-size: 110%; border: 0px solid #999; background-color: #defbe6; border-radius: 4px; box-shadow: 3px 5px 5px -5px rgba(0,0,0,0.75);padding: 5px; font-family:\'Source Code Pro\', monospace;}</style>'+
    '<img src="img_success.jpg" height="150" title="image credit: syarifahbrit / Freepik (www.freepik.com)"><br>'
;

var en_break1_text =
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>You have completed the first of the four tasks in this experiment.</p>'+
    '<p>Take a break if you need to, and then press the \'Continue\' button to go to the next task.</p>'
;
var en_break1_stimulus =
    break_header+en_break1_text
;

var en_break2_text =
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>You have completed two of the four tasks in this experiment.</p>'+
    '<p>Take a break if you need to, and then press the \'Continue\' button to go to the next task.</p>'
;
var en_break2_stimulus =
    break_header+en_break2_text
;

var en_break3_text =
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>You have completed three of the four tasks in this experiment.</p>'+
    '<p>Take a break if you need to, and then press the \'Continue\' button to go to the final task.</p>'
;
var en_break3_stimulus = 
    break_header+en_break3_text
;

var en_submit1_text =
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>You have completed the first of the five tasks in this experiment.</p>'+
    '<p>The task ID you will need to unlock the next task is <span class="code">basil</span></p>'+
    'Click on the button below to submit your responses and return to the login page.'+
    '<p>It may take a moment for your data to finish uploading to the server after you click on the button, so please be patient.</p>'
;
en_submit1_stimulus =
    break_header+en_submit1_text
;
var ko_submit1_text =
    '<span style="font-size:125%; font-weight:bold;">수고하셨습니다!</span>'+
    '<p>실험의 다섯 과제 중 첫번째 과제를 완료하였습니다.</p>'+
    '<p>다음 과제로 진행할 수 있는 과제ID는 다음과 같습니다: <span class="code">basil</span></p>'+
    '아래의 버튼을 클릭하여 답안을 제출하신 뒤, 로그인 페이지로 돌아가주시기 바랍니다.'+
    '<p>답안을 서버에 저장하는 데 시간이 걸릴 수 있으니 잠시만 기다려 주시면 감사하겠습니다.</p>'
;
ko_submit1_stimulus =
    break_header+ko_submit1_text
;
var zh_submit1_text =
    '<span style="font-size:125%; font-weight:bold;">做得好！讚！</span>'+
    '<p>您已經完成這個實驗中5個測驗中的第1個測驗了。</p>'+
    '<p>解鎖下一個測驗的序號是： <span class="code">basil</span></p>'+
    '請點選以下按鈕來提交答案並回到登入畫面。'+
    '<p>按下按鈕後上傳資料到雲端系統可能會需要一點時間，請耐心等待。</p>'
;
zh_submit1_stimulus =
    break_header+zh_submit1_text
;

var en_submit2_text =
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>You have completed two of the five tasks in this experiment.</p>'+
    '<p>The task ID you will need to unlock the next task is <span class="code">coral</span></p>'+
    'Click on the button below to submit your responses and return to the login page.'+
    '<p>It may take a moment for your data to finish uploading to the server after you click on the button, so please be patient.</p>'
;
en_submit2_stimulus =
    break_header+en_submit2_text
;
var ko_submit2_text =
    '<span style="font-size:125%; font-weight:bold;">수고하셨습니다!</span>'+
    '<p>실험의 다섯 과제 중 두번째 과제를 완료하였습니다.</p>'+
    '<p>다음 과제로 진행할 수 있는 과제ID는 다음과 같습니다: <span class="code">coral</span></p>'+
    '아래의 버튼을 클릭하여 답안을 제출하신 뒤, 로그인 페이지로 돌아가주시기 바랍니다.'+
    '<p>답안을 서버에 저장하는 데 시간이 걸릴 수 있으니 잠시만 기다려 주시면 감사하겠습니다.</p>'
;
ko_submit2_stimulus =
    break_header+ko_submit2_text
;
var zh_submit2_text =
    '<span style="font-size:125%; font-weight:bold;">做得好！讚！</span>'+
    '<p>您已經完成這個實驗中5個測驗中的2個測驗了。</p>'+
    '<p>解鎖下一個測驗的序號是： <span class="code">coral</span></p>'+
    '請點選以下按鈕來提交答案並回到登入畫面。'+
    '<p>按下按鈕後上傳資料到雲端系統可能會需要一點時間，請耐心等待。</p>'
;
zh_submit2_stimulus =
    break_header+zh_submit2_text
;

var en_submit3_text =
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>You have completed three of the five tasks in this experiment.</p>'+
    '<p>The task ID you will need to unlock the next task is <span class="code">dream</span></p>'+
    'Click on the button below to submit your responses and return to the login page.'+
    '<p>It may take a moment for your data to finish uploading to the server after you click on the button, so please be patient.</p>'
;
en_submit3_stimulus =
    break_header+en_submit3_text
;
var ko_submit3_text =
    '<span style="font-size:125%; font-weight:bold;">수고하셨습니다!</span>'+
    '<p>실험의 다섯 과제 중 세번째 과제를 완료하였습니다.</p>'+
    '<p>다음 과제로 진행할 수 있는 과제ID는 다음과 같습니다: <span class="code">dream</span></p>'+
    '아래의 버튼을 클릭하여 답안을 제출하신 뒤, 로그인 페이지로 돌아가주시기 바랍니다.'+
    '<p>답안을 서버에 저장하는 데 시간이 걸릴 수 있으니 잠시만 기다려 주시면 감사하겠습니다.</p>'
;
ko_submit3_stimulus =
    break_header+ko_submit3_text
;
var zh_submit3_text =
    '<span style="font-size:125%; font-weight:bold;">做得好！讚！</span>'+
    '<p>您已經完成這個實驗中5個測驗中的3個測驗了。</p>'+
    '<p>解鎖下一個測驗的序號是： <span class="code">dream</span></p>'+
    '請點選以下按鈕來提交答案並回到登入畫面。'+
    '<p>按下按鈕後上傳資料到雲端系統可能會需要一點時間，請耐心等待。</p>'
;
zh_submit3_stimulus =
    break_header+zh_submit3_text
;

var en_submit4_text =
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>You have completed four of the five tasks in this experiment.</p>'+
    '<p>The task ID you will need to unlock the next task is <span class="code">ember</span></p>'+
    'Click on the button below to submit your responses and return to the login page.'+
    '<p>It may take a moment for your data to finish uploading to the server after you click on the button, so please be patient.</p>'
;
en_submit4_stimulus =
    break_header+en_submit4_text
;
var ko_submit4_text =
    '<span style="font-size:125%; font-weight:bold;">수고하셨습니다!</span>'+
    '<p>실험의 다섯 과제 중 네번째 과제를 완료하였습니다.</p>'+
    '<p>다음 과제로 진행할 수 있는 과제ID는 다음과 같습니다: <span class="code">ember</span></p>'+
    '아래의 버튼을 클릭하여 답안을 제출하신 뒤, 로그인 페이지로 돌아가주시기 바랍니다.'+
    '<p>답안을 서버에 저장하는 데 시간이 걸릴 수 있으니 잠시만 기다려 주시면 감사하겠습니다.</p>'
;
ko_submit4_stimulus =
    break_header+ko_submit4_text
;
var zh_submit4_text =
    '<span style="font-size:125%; font-weight:bold;">做得好！讚！</span>'+
    '<p>您已經完成這個實驗中5個測驗中的4個測驗了。</p>'+
    '<p>解鎖下一個測驗的序號是： <span class="code">ember</span></p>'+
    '請點選以下按鈕來提交答案並回到登入畫面。'+
    '<p>按下按鈕後上傳資料到雲端系統可能會需要一點時間，請耐心等待。</p>'
;
zh_submit4_stimulus =
    break_header+zh_submit4_text
;

// exit survey

var en_exit_survey_header =
    '<style>p {text-align:left; spellcheck=false;} input[type="text"] {width:8ch; font-family:\'Source Code Pro\', monospace;} fieldset {border:1px solid #999;box-shadow:2px 2px 5px #999;} legend {background:#fff;text-align:left;font-size:110%;}</style>'+
    '<img src="img_exit.jpg" height="150" title="image credit: vectorjuice / Freepik (www.freepik.com)"><br>'
;
var exit_survey_header =
    '<style>p {text-align:center; spellcheck=false;} input[type="text"] {width:8ch;} fieldset {border:1px solid #999;box-shadow:2px 2px 5px #999;} legend {background:#fff;text-align:left;font-size:110%;}</style>'+
    '<img src="img_success.jpg" height="150" title="image credit: syarifahbrit / Freepik (www.freepik.com)"><br>'
;

var en_exit_survey_text =
    '<span style="font-size:125%; font-weight:bold;">Exit Survey</span>'+
    '<p>What do you think the researcher is trying to find out from this experiment?</p>'+
    '<p style="text-align:left;"><textarea name="exit_purpose" rows="3" style="width:90%;" required></textarea></p>'+
    '<p>If you experienced any difficulties while taking part in the experiment, please describe them here.</p>'+
    '<p style="text-align:left;"><textarea name="exit_difficulties" rows="3" style="width:90%;"></textarea></p>'
;
var en_exit_survey_text_v2 =
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>You have completed the final task in this experiment.</p>'+
    '<p>Please answer the questions below.</p>'+
    '<fieldset><legend><b>Exit Survey</b></legend>'+
    '<p style="text-align:left;">What do you think the researcher is trying to find out from this experiment?</p>'+
    '<p style="text-align:left;"><textarea name="exit_purpose" rows="3" style="width:90%;" required></textarea></p>'+
    '<p style="text-align:left;">If you experienced any difficulties while taking part in the experiment, please describe them here.</p>'+
    '<p style="text-align:left;"><textarea name="exit_difficulties" rows="3" style="width:90%;"></textarea></p>'+
    '</fieldset>'+
    '<p>Click on the button below to submit your responses and enter your payment information.</p>'+
    '<p>It may take a moment for your data to finish uploading to the server after you click on the button, so please be patient.</p>'
;
var ko_exit_survey_text =
    '<span style="font-size:125%; font-weight:bold;">수고하셨습니다!</span>'+
    '<p>실험의 다섯 과제 중 마지막 과제를 완료하였습니다.</p>'+
    '<p>아래에 제시된 질문에 답하여 주십시오.</p>'+
    '<fieldset><legend><b>실험 후 설문조사</b></legend>'+
    '<p style="text-align:left;">연구자가 본 실험을 통해 알아내고자 하는 것은 무엇이라고 생각하십니까?</p>'+
    '<p style="text-align:left;"><textarea name="exit_purpose" rows="3" style="width:90%;" required></textarea></p>'+
    '<p style="text-align:left;">만약 실험을 진행하면서 어려운 점이 있었다면 아래에 설명하여 주시면 감사하겠습니다.</p>'+
    '<p style="text-align:left;"><textarea name="exit_difficulties" rows="3" style="width:90%;"></textarea></p>'+
    '</fieldset>'+
    '<p>아래의 버튼을 클릭하여 답안을 제출하고, 실험 참가비 지급 관련 정보를 입력하여 주시기 바랍니다.</p>'+
    '<p>버튼을 누른 뒤에도 답안을 서버에 저장하는 데 시간이 걸릴 수 있으니 잠시만 기다려 주시면 감사하겠습니다.</p>'
;
var zh_exit_survey_text =
    '<span style="font-size:125%; font-weight:bold;">做得好！讚！</span>'+
    '<p>您已經完成這個實驗中所有測驗了。</p>'+
    '<p>請回答以下問題。</p>'+
    '<fieldset><legend><b>意見回饋</b></legend>'+
    '<p style="text-align:left;">您認為這個實驗的研究者在研究什麼？</p>'+
    '<p style="text-align:left;"><textarea name="exit_purpose" rows="3" style="width:90%;" required></textarea></p>'+
    '<p style="text-align:left;">如果您在實驗中有經歷任何困難，請在這裡描述。</p>'+
    '<p style="text-align:left;"><textarea name="exit_difficulties" rows="3" style="width:90%;"></textarea></p>'+
    '</fieldset>'+
    '<p>請點選以下按鈕來提交您的回覆並進入匯款畫面。</p>'+
    '<p>按下按鈕後上傳資料到雲端系統可能會需要一點時間，請耐心等待。</p>'
;
var ko_exit_survey_text_lbc =
    '<span style="font-size:125%; font-weight:bold;">수고하셨습니다!</span>'+
    '<p>실험의 다섯 과제 중 마지막 과제를 완료하였습니다.</p>'+
    '<p>아래에 제시된 질문에 답하여 주십시오.</p>'+
    '<fieldset><legend><b>실험 후 설문조사</b></legend>'+
    '<p style="text-align:left;">연구자가 본 실험을 통해 알아내고자 하는 것은 무엇이라고 생각하십니까?</p>'+
    '<p style="text-align:left;"><textarea name="exit_purpose" rows="3" style="width:90%;" required></textarea></p>'+
    '<p style="text-align:left;">만약 실험을 진행하면서 어려운 점이 있었다면 아래에 설명하여 주시면 감사하겠습니다.</p>'+
    '<p style="text-align:left;"><textarea name="exit_difficulties" rows="3" style="width:90%;"></textarea></p>'+
    '</fieldset>'+
    '<p>아래의 버튼을 클릭하여 답안을 제출하여 주십시오.</p>'+
    '<p>버튼을 누른 뒤에도 답안을 서버에 저장하는 데 시간이 걸릴 수 있으니 잠시만 기다려 주시면 감사하겠습니다.</p>'
;

var en_exit_survey_html =
    en_exit_survey_header+en_exit_survey_text
;
var ko_exit_survey_html =
    exit_survey_header+ko_exit_survey_text
;
var zh_exit_survey_html =
    exit_survey_header+zh_exit_survey_text
;
var ko_exit_survey_html_lbc =
    exit_survey_header+ko_exit_survey_text_lbc
;

// end message

var en_end_message_text = 
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>You have completed the final task. Click on the link below to return to the Prolific website and finish the experiment.</p>'+
    '<p>It may take a moment for your data to finish uploading to the server before you are redirected to Prolific.</p>'
;
var lbc_end_message_text = 
    '<span style="font-size:125%; font-weight:bold;">Nice work!</span>'+
    '<p>You have completed the final task. Click on the link below to submit your responses and finish the experiment.</p>'+
    '<p>It may take a moment for your data to finish saving, so don\'t close this tab until you\'ve been redirected to the LBC website.</p>'
;

var en_end_message_stimulus = 
    break_header+en_end_message_text
;
var lbc_end_message_stimulus = 
    break_header+lbc_end_message_text
;

var en_end_message_button = [
    '<span style="font-size:125%;">Click here to return to Prolific</span>'
];
var lbc_end_message_button = [
    '<span style="font-size:125%;">Click here to return to the LBC website</span>'
];

// loading message

var loading_header = 
    '<img src="img_loading.jpg" height="115" title="image credit: tenor (tenor.com)"><br>'
;

var en_loading_text = '<p>Loading. Please wait.</p>';
var ko_loading_text = '<p>로딩중입니다. 잠시만 기다려 주십시오.</p>';
var zh_loading_text = '<p>下載中，請稍等。</p>';

var en_loading_message = loading_header + en_loading_text;
var ko_loading_message = loading_header + ko_loading_text;
var zh_loading_message = loading_header + zh_loading_text;

// *** chopping block ***

// uploading message

/*

var loading_header = 
    '<img src="img_loading.jpg" height="115" title="image credit: stories / Freepik (www.freepik.com)"><br>'
;

var en_loading_text =
    '<span style="font-size:115%;">Please wait while your responses are being uploaded to the server</span>'
;
var ko_loading_text =
    '<span style="font-size:115%;">답안이 서버에 저장될 때까지 기다려 주시기 바랍니다</span>'
;
var zh_loading_text =
    '<span style="font-size:115%;">PLEASE WAIT WHILE YOUR RESPONSES ARE BEING UPLOADED TO THE SERVER</span>'
;

var en_loading_stimulus = 
    loading_header+en_loading_text
;
var ko_loading_stimulus = 
    loading_header+ko_loading_text
;
var zh_loading_stimulus = 
    loading_header+zh_loading_text
;

*/