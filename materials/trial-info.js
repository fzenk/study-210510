// Title: Trial information for experiment 210510
// Author: Fred Zenker
// Date: June 27, 2021

// --------------------------------------------------------
// key to abbreviations
// --------------------------------------------------------

// DO: direct object
// SU: subject
// dr: self-paced reading and acceptability judgment stimuli for the direct object relative clause sub-study
// sr: self-paced reading and acceptability judgment stimuli for the subject relative cluase sub-study
// dp: elicited production stimuli for the direct object relative clause sub-study
// sp: elicited production stimuli for the subject relative clause sub-study
// fr: fillers for self-paced reading and acceptability judgment tasks
// fp: fillers for elicited production tasks
// prac: practice
// cond: condition
// coda: long time phrase for self-paced reading tasks
// time: short time phrase for acceptability judgment tasks
// eng: stimulus for English task
// kor: stimulus for Korean task
// zho: stimulus for Mandarin task

// --------------------------------------------------------
// DO study: critical items for reading tasks
// --------------------------------------------------------

var dr01 = {
    type: 'critical',
    item: 'item01',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_center: 'arrested',
    choices1: jsPsych.randomization.shuffle(['Mary', 'I', 'the man', 'the detectives']),
    correct1: 'the detectives',
    choices2: jsPsych.randomization.shuffle(['Mary', 'me', 'the man', 'the detectives']),
    correct2: 'the man'
};
var dr01_cond1 = {
    eng: 'I think Mary knows the man that these detectives arrested',
    kor: '내가 생각하기에 민지는 지난주에 이 형사들이 체포한 남자를 안다.',
    zho: '我覺得黃麗認識那個這些偵探上個星期逮捕的男人。',
    cond: 'cond1',
    ...dr01
};
var dr01_cond2 = {
    eng: 'Mary knows the man that I think these detectives arrested',
    kor: '민지는 지난주에 이 형사들이 체포했다고 내가 생각하는 남자를 안다.',
    zho: '黃麗認識那個我覺得這些偵探上個星期逮捕的男人',
    cond: 'cond2',
    ...dr01
};
var dr01_cond3 = {
    eng: 'Mary knows the man that I wonder which detectives arrested',
    kor: '민지는 지난주에 어느 형사들이 체포했는지 내가 궁금해하는 남자를 안다.',
    zho: '黃麗認識那個我好奇哪些偵探上個星期逮捕的男人。',
    cond: 'cond3',
    ...dr01
};
var dr01_cond4 = {
    eng: 'I think Mary knows the man that these detectives arrested him',
    kor: '내가 생각하기에 민지는 지난주에 이 형사들이 그를 체포한 남자를 안다.',
    zho: '我覺得黃麗認識那個這些偵探上個星期逮捕他的男人。',
    cond: 'cond4',
    ...dr01
};
var dr01_cond5 = {
    eng: 'Mary knows the man that I think these detectives arrested him',
    kor: '민지는 지난주에 이 형사들이 그를 체포했다고 내가 생각하는 남자를 안다.',
    zho: '黃麗認識那個我覺得這些偵探上個星期逮捕他的男人。',
    cond: 'cond5',
    ...dr01
};
var dr01_cond6 = {
    eng: 'Mary knows the man that I wonder which detectives arrested him',
    kor: '민지는 지난주에 어느 형사들이 그를 체포했는지 내가 궁금해하는 남자를 안다.',
    zho: '黃麗認識那個我好奇哪些偵探上個星期逮捕他的男人。',
    cond: 'cond6',
    ...dr01
};

var dr02 = {
    type: 'critical',
    item: 'item02',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_center: 'arrested',
    choices1: jsPsych.randomization.shuffle(['John', 'I', 'the woman', 'the officers']),
    correct1: 'the officers',
    choices2: jsPsych.randomization.shuffle(['John', 'me', 'the woman', 'the officers']),
    correct2: 'the woman'
};
var dr02_cond1 = {
    eng: 'I think John knows the woman that these officers arrested',
    kor: '내가 생각하기에 철수는 지난주에 이 경찰들이 체포한 여자를 안다.',
    zho: '我覺得張偉認識那個這些警官上個星期逮捕的女人。',
    cond: 'cond1',
    ...dr02
};
var dr02_cond2 = {
    eng: 'John knows the woman that I think these officers arrested',
    kor: '철수는 지난주에 이 경찰들이 체포했다고 내가 생각하는 여자를 안다.',
    zho: '張偉認識那個我覺得這些警官上個星期逮捕的女人。',
    cond: 'cond2',
    ...dr02
};
var dr02_cond3 = {
    eng: 'John knows the woman that I wonder which officers arrested',
    kor: '철수는 지난주에 어느 경찰들이 체포했는지 내가 궁금해하는 여자를 안다.',
    zho: '張偉認識那個我好奇哪些警官上個星期逮捕的女人。',
    cond: 'cond3',
    ...dr02
};
var dr02_cond4 = {
    eng: 'I think John knows the woman that these officers arrested her',
    kor: '내가 생각하기에 철수는 지난주에 이 경찰들이 그녀를 체포한 여자를 안다.',
    zho: '我覺得張偉認識那個這些警官上個星期逮捕她的女人。',
    cond: 'cond4',
    ...dr02
};
var dr02_cond5 = {
    eng: 'John knows the woman that I think these officers arrested her',
    kor: '철수는 지난주에 이 경찰들이 그녀를 체포했다고 내가 생각하는 여자를 안다.',
    zho: '張偉認識那個我覺得這些警官上個星期逮捕她的女人。',
    cond: 'cond5',
    ...dr02
};
var dr02_cond6 = {
    eng: 'John knows the woman that I wonder which officers arrested her',
    kor: '철수는 지난주에 어느 경찰들이 그녀를 체포했는지 내가 궁금해하는 여자를 안다.',
    zho: '張偉認識那個我好奇哪些警官上個星期逮捕她的女人。',
    cond: 'cond6',
    ...dr02
};

var dr03 = {
    type: 'critical',
    item: 'item03',
    coda: ' at the end of the night.',
    time: ' last night.',
    text_center: 'caught',
    choices1: jsPsych.randomization.shuffle(['Lisa', 'I', 'the man', 'the detectives']),
    correct1: 'the detectives',
    choices2: jsPsych.randomization.shuffle(['Lisa', 'me', 'the man', 'the detectives']),
    correct2: 'the man'
};
var dr03_cond1 = {
    eng: 'I think Lisa knows the man that these detectives caught',
    kor: '내가 생각하기에 은지는 어젯밤에 이 형사들이 잡은 남자를 안다.',
    zho: '我覺得李娜認識那個這些偵探昨晚抓住的男人。',
    cond: 'cond1',
    ...dr03
};
var dr03_cond2 = {
    eng: 'Lisa knows the man that I think these detectives caught',
    kor: '은지는 어젯밤에 이 형사들이 잡았다고 내가 생각하는 남자를 안다.',
    zho: '李娜認識那個我覺得這些偵探昨晚抓住的男人。',
    cond: 'cond2',
    ...dr03
};
var dr03_cond3 = {
    eng: 'Lisa knows the man that I wonder which detectives caught',
    kor: '은지는 어젯밤에 어느 형사들이 잡았는지 내가 궁금해하는 남자를 안다.',
    zho: '李娜認識那個我好奇哪些偵探昨晚抓住的男人。',
    cond: 'cond3',
    ...dr03
};
var dr03_cond4 = {
    eng: 'I think Lisa knows the man that these detectives caught him',
    kor: '내가 생각하기에 은지는 어젯밤에 이 형사들이 그를 잡은 남자를 안다.',
    zho: '我覺得李娜認識那個這些偵探昨晚抓住他的男人。',
    cond: 'cond4',
    ...dr03
};
var dr03_cond5 = {
    eng: 'Lisa knows the man that I think these detectives caught him',
    kor: '은지는 어젯밤에 이 형사들이 그를 잡았다고 내가 생각하는 남자를 안다.',
    zho: '李娜認識那個我覺得這些偵探昨晚抓住他的男人。',
    cond: 'cond5',
    ...dr03
};
var dr03_cond6 = {
    eng: 'Lisa knows the man that I wonder which detectives caught him',
    kor: '은지는 어젯밤에 어느 형사들이 그를 잡았는지 내가 궁금해하는 남자를 안다.',
    zho: '李娜認識那個我好奇哪些偵探昨晚抓住他的男人。',
    cond: 'cond6',
    ...dr03
};

var dr04 = {
    type: 'critical',
    item: 'item04',
    coda: ' at the beginning of the night.',
    time: ' last night.',
    text_center: 'caught',
    choices1: jsPsych.randomization.shuffle(['Bill', 'I', 'the woman', 'the officials']),
    correct1: 'the officials',
    choices2: jsPsych.randomization.shuffle(['Bill', 'me', 'the woman', 'the officials']),
    correct2: 'the woman'
};
var dr04_cond1 = {
    eng: 'I think Bill knows the woman that these officials caught',
    kor: '내가 생각하기에 우진이는 어젯밤에 이 관계자들이 잡은 여자를 안다.',
    zho: '我覺得吳強認識那個這些官員昨晚抓住的女人。',
    cond: 'cond1',
    ...dr04
};
var dr04_cond2 = {
    eng: 'Bill knows the woman that I think these officials caught',
    kor: '우진이는 어젯밤에 이 관계자들이 잡았다고 내가 생각하는 여자를 안다.',
    zho: '吳強認識那個我覺得這些官員昨晚抓住的女人。',
    cond: 'cond2',
    ...dr04
};
var dr04_cond3 = {
    eng: 'Bill knows the woman that I wonder which officials caught',
    kor: '우진이는 어젯밤에 어느 관계자들이 잡았는지 내가 궁금해하는 여자를 안다.',
    zho: '吳強認識那個我好奇哪些官員昨晚抓住的女人。',
    cond: 'cond3',
    ...dr04
};
var dr04_cond4 = {
    eng: 'I think Bill knows the woman that these officials caught her',
    kor: '내가 생각하기에 우진이는 어젯밤에 이 관계자들이 그녀를 잡은 여자를 안다.',
    zho: '我覺得吳強認識那個這些官員昨晚抓住她的女人。',
    cond: 'cond4',
    ...dr04
};
var dr04_cond5 = {
    eng: 'Bill knows the woman that I think these officials caught her',
    kor: '우진이는 어젯밤에 이 관계자들이 그녀를 잡았다고 내가 생각하는 여자를 안다.',
    zho: '吳強認識那個我覺得這些官員昨晚抓住她的女人。',
    cond: 'cond5',
    ...dr04
};
var dr04_cond6 = {
    eng: 'Bill knows the woman that I wonder which officials caught her',
    kor: '우진이는 어젯밤에 어느 관계자들이 그녀를 잡았는지 내가 궁금해하는 여자를 안다.',
    zho: '吳強認識那個我好奇哪些官員昨晚抓住她的女人。',
    cond: 'cond6',
    ...dr04
};

var dr05 = {
    type: 'critical',
    item: 'item05',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_center: 'defended',
    choices1: jsPsych.randomization.shuffle(['Tina', 'I', 'the man', 'the lawyers']),
    correct1: 'the lawyers',
    choices2: jsPsych.randomization.shuffle(['Tina', 'me', 'the man', 'the lawyers']),
    correct2: 'the man'
};
var dr05_cond1 = {
    eng: 'I think Tina knows the man that these lawyers defended',
    kor: '내가 생각하기에 수아는 지난달에 이 변호사들이 변호한 남자를 안다.',
    zho: '我覺得趙悅認識那個這些律師上個月幫忙辯護的男人。',
    cond: 'cond1',
    ...dr05
};
var dr05_cond2 = {
    eng: 'Tina knows the man that I think these lawyers defended',
    kor: '수아는 지난달에 이 변호사들이 변호했다고 내가 생각하는 남자를 안다.',
    zho: '趙悅認識那個我覺得這些律師上個月幫忙辯護的男人。',
    cond: 'cond2',
    ...dr05
};
var dr05_cond3 = {
    eng: 'Tina knows the man that I wonder which lawyers defended',
    kor: '수아는 지난달에 어느 변호사들이 변호했는지 내가 궁금해하는 남자를 안다.',
    zho: '趙悅認識那個我好奇哪些律師上個月幫忙辯護的男人。',
    cond: 'cond3',
    ...dr05
};
var dr05_cond4 = {
    eng: 'I think Tina knows the man that these lawyers defended him',
    kor: '내가 생각하기에 수아는 지난달에 이 변호사들이 그를 변호한 남자를 안다.',
    zho: '我覺得趙悅認識那個這些律師上個月幫忙辯護他的男人。',
    cond: 'cond4',
    ...dr05
};
var dr05_cond5 = {
    eng: 'Tina knows the man that I think these lawyers defended him',
    kor: '수아는 지난달에 이 변호사들이 그를 변호했다고 내가 생각하는 남자를 안다.',
    zho: '趙悅認識那個我覺得這些律師上個月幫忙辯護他的男人。',
    cond: 'cond5',
    ...dr05
};
var dr05_cond6 = {
    eng: 'Tina knows the man that I wonder which lawyers defended him',
    kor: '수아는 지난달에 어느 변호사들이 그를 변호했는지 내가 궁금해하는 남자를 안다.',
    zho: '趙悅認識那個我好奇哪些律師上個月幫忙辯護他的男人。',
    cond: 'cond6',
    ...dr05
};

var dr06 = {
    type: 'critical',
    item: 'item06',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_center: 'defended',
    choices1: jsPsych.randomization.shuffle(['Mike', 'I', 'the woman', 'the firm']),
    correct1: 'the firm',
    choices2: jsPsych.randomization.shuffle(['Mike', 'me', 'the woman', 'the firm']),
    correct2: 'the woman'
};
var dr06_cond1 = {
    eng: 'I think Mike knows the woman that this firm defended',
    kor: '내가 생각하기에 서준이는 지난달에 이 로펌이 변호한 여자를 안다.',
    zho: '我覺得楊毅認識那個這家律師事務所上個月幫忙辯護的女人。',
    cond: 'cond1',
    ...dr06
};
var dr06_cond2 = {
    eng: 'Mike knows the woman that I think this firm defended',
    kor: '서준이는 지난달에 이 로펌이 변호했다고 내가 생각하는 여자를 안다.',
    zho: '楊毅認識那個我覺得這家律師事務所上個月幫忙辯護的女人。',
    cond: 'cond2',
    ...dr06
};
var dr06_cond3 = {
    eng: 'Mike knows the woman that I wonder which firm defended',
    kor: '서준이는 지난달에 어느 로펌이 변호했는지 내가 궁금해하는 여자를 안다.',
    zho: '楊毅認識那個我好奇哪家律師事務所上個月幫忙辯護的女人。',
    cond: 'cond3',
    ...dr06
};
var dr06_cond4 = {
    eng: 'I think Mike knows the woman that this firm defended her',
    kor: '내가 생각하기에 서준이는 지난달에 이 로펌이 그녀를 변호한 여자를 안다.',
    zho: '我覺得楊毅認識那個這家律師事務所上個月幫忙辯護她的女人。',
    cond: 'cond4',
    ...dr06
};
var dr06_cond5 = {
    eng: 'Mike knows the woman that I think this firm defended her',
    kor: '서준이는 지난달에 이 로펌이 그녀를 변호했다고 내가 생각하는 여자를 안다.',
    zho: '楊毅認識那個我覺得這家律師事務所上個月幫忙辯護她的女人。',
    cond: 'cond5',
    ...dr06
};
var dr06_cond6 = {
    eng: 'Mike knows the woman that I wonder which firm defended her',
    kor: '서준이는 지난달에 어느 로펌이 그녀를 변호했는지 내가 궁금해하는 여자를 안다.',
    zho: '楊毅認識那個我好奇哪家律師事務所上個月幫忙辯護她的女人。',
    cond: 'cond6',
    ...dr06
};

var dr07 = {
    type: 'critical',
    item: 'item07',
    coda: ' at the end of the year.',
    time: ' last year.',
    text_center: 'employed',
    choices1: jsPsych.randomization.shuffle(['Judy', 'I', 'the man', 'the restaurant']),
    correct1: 'the restaurant',
    choices2: jsPsych.randomization.shuffle(['Judy', 'me', 'the man', 'the restaurant']),
    correct2: 'the man'
};
var dr07_cond1 = {
    eng: 'I think Judy knows the man that this restaurant employed',
    kor: '내가 생각하기에 지은이는 작년에 이 식당이 고용한 남자를 안다.',
    zho: '我覺得周靜認識那個這家餐廳去年聘用的男人。',
    cond: 'cond1',
    ...dr07
};
var dr07_cond2 = {
    eng: 'Judy knows the man that I think this restaurant employed',
    kor: '지은이는 작년에 이 식당이 고용했다고 내가 생각하는 남자를 안다.',
    zho: '周靜認識那個我覺得這家餐廳去年聘用的男人。',
    cond: 'cond2',
    ...dr07
};
var dr07_cond3 = {
    eng: 'Judy knows the man that I wonder which restaurant employed',
    kor: '지은이는 작년에 어느 식당이 고용했는지 내가 궁금해하는 남자를 안다.',
    zho: '周靜認識那個我好奇哪家餐廳去年聘用的男人。',
    cond: 'cond3',
    ...dr07
};
var dr07_cond4 = {
    eng: 'I think Judy knows the man that this restaurant employed him',
    kor: '내가 생각하기에 지은이는 작년에 이 식당이 그를 고용한 남자를 안다.',
    zho: '我覺得周靜認識那個這家餐廳去年聘用他的男人。',
    cond: 'cond4',
    ...dr07
};
var dr07_cond5 = {
    eng: 'Judy knows the man that I think this restaurant employed him',
    kor: '지은이는 작년에 이 식당이 그를 고용했다고 내가 생각하는 남자를 안다.',
    zho: '周靜認識那個我覺得這家餐廳去年聘用他的男人。',
    cond: 'cond5',
    ...dr07
};
var dr07_cond6 = {
    eng: 'Judy knows the man that I wonder which restaurant employed him',
    kor: '지은이는 작년에 어느 식당이 그를 고용했는지 내가 궁금해하는 남자를 안다.',
    zho: '周靜認識那個我好奇哪家餐廳去年聘用他的男人。',
    cond: 'cond6',
    ...dr07
};

var dr08 = {
    type: 'critical',
    item: 'item08',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_center: 'employed',
    choices1: jsPsych.randomization.shuffle(['Luke', 'I', 'the woman', 'the company']),
    correct1: 'the company',
    choices2: jsPsych.randomization.shuffle(['Luke', 'me', 'the woman', 'the company']),
    correct2: 'the woman'
};
var dr08_cond1 = {
    eng: 'I think Luke knows the woman that this company employed',
    kor: '내가 생각하기에 현우는 작년에 이 회사가 고용한 여자를 안다.',
    zho: '我覺得陳傑認識那個這家公司去年聘用的女人。',
    cond: 'cond1',
    ...dr08
};
var dr08_cond2 = {
    eng: 'Luke knows the woman that I think this company employed',
    kor: '현우는 작년에 이 회사가 고용했다고 내가 생각하는 여자를 안다.',
    zho: '陳傑認識那個我覺得這家公司去年聘用的女人。',
    cond: 'cond2',
    ...dr08
};
var dr08_cond3 = {
    eng: 'Luke knows the woman that I wonder which company employed',
    kor: '현우는 작년에 어느 회사가 고용했는지 내가 궁금해하는 여자를 안다.',
    zho: '陳傑認識那個我好奇哪家公司去年聘用的女人。',
    cond: 'cond3',
    ...dr08
};
var dr08_cond4 = {
    eng: 'I think Luke knows the woman that this company employed her',
    kor: '내가 생각하기에 현우는 작년에 이 회사가 그녀를 고용한 여자를 안다.',
    zho: '我覺得陳傑認識那個這家公司去年聘用她的女人。',
    cond: 'cond4',
    ...dr08
};
var dr08_cond5 = {
    eng: 'Luke knows the woman that I think this company employed her',
    kor: '현우는 작년에 이 회사가 그녀를 고용했다고 내가 생각하는 여자를 안다.',
    zho: '陳傑認識那個我覺得這家公司去年聘用她的女人。',
    cond: 'cond5',
    ...dr08
};
var dr08_cond6 = {
    eng: 'Luke knows the woman that I wonder which company employed her',
    kor: '현우는 작년에 어느 회사가 그녀를 고용했는지 내가 궁금해하는 여자를 안다.',
    zho: '陳傑認識那個我好奇哪家公司去年聘用她的女人。',
    cond: 'cond6',
    ...dr08
};

var dr09 = {
    type: 'critical',
    item: 'item09',
    coda: ' at the beginning of the night.',
    time: ' last night.',
    text_center: 'examined',
    choices1: jsPsych.randomization.shuffle(['Anna', 'I', 'the man', 'the nurses']),
    correct1: 'the nurses',
    choices2: jsPsych.randomization.shuffle(['Anna', 'me', 'the man', 'the nurses']),
    correct2: 'the man'
};
var dr09_cond1 = {
    eng: 'I think Anna knows the man that these nurses examined',
    kor: '내가 생각하기에 하은이는 어젯밤에 이 간호사들이 진찰한 남자를 안다.',
    zho: '我覺得王芳認識那個這些護理師昨晚檢查的男人。',
    cond: 'cond1',
    ...dr09
};
var dr09_cond2 = {
    eng: 'Anna knows the man that I think these nurses examined',
    kor: '하은이는 어젯밤에 이 간호사들이 진찰했다고 내가 생각하는 남자를 안다.',
    zho: '王芳認識那個我覺得這些護理師昨晚檢查的男人。',
    cond: 'cond2',
    ...dr09
};
var dr09_cond3 = {
    eng: 'Anna knows the man that I wonder which nurses examined',
    kor: '하은이는 어젯밤에 어느 간호사들이 진찰했는지 내가 궁금해하는 남자를 안다.',
    zho: '王芳認識那個我好奇哪些護理師昨晚檢查的男人。',
    cond: 'cond3',
    ...dr09
};
var dr09_cond4 = {
    eng: 'I think Anna knows the man that these nurses examined him',
    kor: '내가 생각하기에 하은이는 어젯밤에 이 간호사들이 그를 진찰한 남자를 안다.',
    zho: '我覺得王芳認識那個這些護理師昨晚檢查他的男人。',
    cond: 'cond4',
    ...dr09
};
var dr09_cond5 = {
    eng: 'Anna knows the man that I think these nurses examined him',
    kor: '하은이는 어젯밤에 이 간호사들이 그를 진찰했다고 내가 생각하는 남자를 안다.',
    zho: '王芳認識那個我覺得這些護理師昨晚檢查他的男人。',
    cond: 'cond5',
    ...dr09
};
var dr09_cond6 = {
    eng: 'Anna knows the man that I wonder which nurses examined him',
    kor: '하은이는 어젯밤에 어느 간호사들이 그를 진찰했는지 내가 궁금해하는 남자를 안다.',
    zho: '王芳認識那個我好奇哪些護理師昨晚檢查他的男人。',
    cond: 'cond6',
    ...dr09
};

var dr10 = {
    type: 'critical',
    item: 'item10',
    coda: ' at the end of the night.',
    time: ' last night.',
    text_center: 'examined',
    choices1: jsPsych.randomization.shuffle(['Dave', 'I', 'the woman', 'the doctors']),
    correct1: 'the doctors',
    choices2: jsPsych.randomization.shuffle(['Dave', 'me', 'the woman', 'the doctors']),
    correct2: 'the woman'
};
var dr10_cond1 = {
    eng: 'I think Dave knows the woman that these doctors examined',
    kor: '내가 생각하기에 도윤이는 어젯밤에 이 의사들이 진찰한 여자를 안다.',
    zho: '我覺得劉浩認識那個這些醫生昨晚檢查的女人。',
    cond: 'cond1',
    ...dr10
};
var dr10_cond2 = {
    eng: 'Dave knows the woman that I think these doctors examined',
    kor: '도윤이는 어젯밤에 이 의사들이 진찰했다고 내가 생각하는 여자를 안다.',
    zho: '劉浩認識那個我覺得這些醫生昨晚檢查的女人',
    cond: 'cond2',
    ...dr10
};
var dr10_cond3 = {
    eng: 'Dave knows the woman that I wonder which doctors examined',
    kor: '도윤이는 어젯밤에 어느 의사들이 진찰했는지 내가 궁금해하는 여자를 안다.',
    zho: '劉浩認識那個我好奇哪些醫生昨晚檢查的女人。',
    cond: 'cond3',
    ...dr10
};
var dr10_cond4 = {
    eng: 'I think Dave knows the woman that these doctors examined her',
    kor: '내가 생각하기에 도윤이는 어젯밤에 이 의사들이 그녀를 진찰한 여자를 안다.',
    zho: '我覺得劉浩認識那個這些醫生昨晚檢查她的女人。',
    cond: 'cond4',
    ...dr10
};
var dr10_cond5 = {
    eng: 'Dave knows the woman that I think these doctors examined her',
    kor: '도윤이는 어젯밤에 이 의사들이 그녀를 진찰했다고 내가 생각하는 여자를 안다.',
    zho: '劉浩認識那個我覺得這些醫生昨晚檢查她的女人。',
    cond: 'cond5',
    ...dr10
};
var dr10_cond6 = {
    eng: 'Dave knows the woman that I wonder which doctors examined her',
    kor: '도윤이는 어젯밤에 어느 의사들이 그녀를 진찰했는지 내가 궁금해하는 여자를 안다.',
    zho: '劉浩認識那個我好奇哪些醫生昨晚檢查她的女人。',
    cond: 'cond6',
    ...dr10
};

var dr11 = {
    type: 'critical',
    item: 'item11',
    coda: ' at the end of the year.',
    time: ' last year.',
    text_center: 'fired',
    choices1: jsPsych.randomization.shuffle(['Mary', 'I', 'the man', 'the company']),
    correct1: 'the company',
    choices2: jsPsych.randomization.shuffle(['Mary', 'me', 'the man', 'the company']),
    correct2: 'the man'
};
var dr11_cond1 = {
    eng: 'I think Mary knows the man that this company fired',
    kor: '내가 생각하기에 민지는 작년에 이 회사가 해고한 남자를 안다.',
    zho: '我覺得黃麗認識那個這家公司去年解雇的男人。',
    cond: 'cond1',
    ...dr11
};
var dr11_cond2 = {
    eng: 'Mary knows the man that I think this company fired',
    kor: '민지는 작년에 이 회사가 해고했다고 내가 생각하는 남자를 안다.',
    zho: '黃麗認識那個我覺得這家公司去年解雇的男人。',
    cond: 'cond2',
    ...dr11
};
var dr11_cond3 = {
    eng: 'Mary knows the man that I wonder which company fired',
    kor: '민지는 작년에 어느 회사가 해고했는지 내가 궁금해하는 남자를 안다.',
    zho: '黃麗認識那個我好奇哪家公司去年解雇的男人。',
    cond: 'cond3',
    ...dr11
};
var dr11_cond4 = {
    eng: 'I think Mary knows the man that this company fired him',
    kor: '내가 생각하기에 민지는 작년에 이 회사가 그를 해고한 남자를 안다.',
    zho: '我覺得黃麗認識那個這家公司去年解雇他的男人。',
    cond: 'cond4',
    ...dr11
};
var dr11_cond5 = {
    eng: 'Mary knows the man that I think this company fired him',
    kor: '민지는 작년에 이 회사가 그를 해고했다고 내가 생각하는 남자를 안다.',
    zho: '黃麗認識那個我覺得這家公司去年解雇他的男人。',
    cond: 'cond5',
    ...dr11
};
var dr11_cond6 = {
    eng: 'Mary knows the man that I wonder which company fired him',
    kor: '민지는 작년에 어느 회사가 그를 해고했는지 내가 궁금해하는 남자를 안다.',
    zho: '黃麗認識那個我好奇哪家公司去年解雇他的男人。',
    cond: 'cond6',
    ...dr11
};

var dr12 = {
    type: 'critical',
    item: 'item12',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_center: 'fired',
    choices1: jsPsych.randomization.shuffle(['John', 'I', 'the woman', 'the business']),
    correct1: 'the business',
    choices2: jsPsych.randomization.shuffle(['John', 'me', 'the woman', 'the business']),
    correct2: 'the woman'
};
var dr12_cond1 = {
    eng: 'I think John knows the woman that this business fired',
    kor: '내가 생각하기에 철수는 작년에 이 사업장이 해고한 여자를 안다.',
    zho: '我覺得張偉認識那個這家企業去年解雇的女人。',
    cond: 'cond1',
    ...dr12
};
var dr12_cond2 = {
    eng: 'John knows the woman that I think this business fired',
    kor: '철수는 작년에 이 사업장이 해고했다고 내가 생각하는 여자를 안다.',
    zho: '張偉認識那個我覺得這家企業去年解雇的女人。',
    cond: 'cond2',
    ...dr12
};
var dr12_cond3 = {
    eng: 'John knows the woman that I wonder which business fired',
    kor: '철수는 작년에 어느 사업장이 해고했는지 내가 궁금해하는 여자를 안다.',
    zho: '張偉認識那個我好奇哪家企業去年解雇的女人。',
    cond: 'cond3',
    ...dr12
};
var dr12_cond4 = {
    eng: 'I think John knows the woman that this business fired her',
    kor: '내가 생각하기에 철수는 작년에 이 사업장이 그녀를 해고한 여자를 안다.',
    zho: '我覺得張偉認識那個這家企業去年解雇她的女人。',
    cond: 'cond4',
    ...dr12
};
var dr12_cond5 = {
    eng: 'John knows the woman that I think this business fired her',
    kor: '철수는 작년에 이 사업장이 그녀를 해고했다고 내가 생각하는 여자를 안다.',
    zho: '張偉認識那個我覺得這家企業去年解雇她的女人。',
    cond: 'cond5',
    ...dr12
};
var dr12_cond6 = {
    eng: 'John knows the woman that I wonder which business fired her',
    kor: '철수는 작년에 어느 사업장이 그녀를 해고했는지 내가 궁금해하는 여자를 안다.',
    zho: '張偉認識那個我好奇哪家企業去年解雇她的女人。',
    cond: 'cond6',
    ...dr12
};

var dr13 = {
    type: 'critical',
    item: 'item13',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_center: 'hired',
    choices1: jsPsych.randomization.shuffle(['Lisa', 'I', 'the man', 'the restaurant']),
    correct1: 'the restaurant',
    choices2: jsPsych.randomization.shuffle(['Lisa', 'me', 'the man', 'the restaurant']),
    correct2: 'the man'
};
var dr13_cond1 = {
    eng: 'I think Lisa knows the man that this restaurant hired',
    kor: '내가 생각하기에 은지는 작년에 이 식당이 채용한 남자를 안다.',
    zho: '我覺得李娜認識那個這家餐廳去年雇用的男人。',
    cond: 'cond1',
    ...dr13
};
var dr13_cond2 = {
    eng: 'Lisa knows the man that I think this restaurant hired',
    kor: '은지는 작년에 이 식당이 채용했다고 내가 생각하는 남자를 안다.',
    zho: '李娜認識那個我覺得這家餐廳去年雇用的男人。',
    cond: 'cond2',
    ...dr13
};
var dr13_cond3 = {
    eng: 'Lisa knows the man that I wonder which restaurant hired',
    kor: '은지는 작년에 어느 식당이 채용했는지 내가 궁금해하는 남자를 안다.',
    zho: '李娜認識那個我好奇哪家餐廳去年雇用的男人。',
    cond: 'cond3',
    ...dr13
};
var dr13_cond4 = {
    eng: 'I think Lisa knows the man that this restaurant hired him',
    kor: '내가 생각하기에 은지는 작년에 이 식당이 그를 채용한 남자를 안다.',
    zho: '我覺得李娜認識那個這家餐廳去年雇用他的男人。',
    cond: 'cond4',
    ...dr13
};
var dr13_cond5 = {
    eng: 'Lisa knows the man that I think this restaurant hired him',
    kor: '은지는 작년에 이 식당이 그를 채용했다고 내가 생각하는 남자를 안다.',
    zho: '李娜認識那個我覺得這家餐廳去年雇用他的男人。',
    cond: 'cond5',
    ...dr13
};
var dr13_cond6 = {
    eng: 'Lisa knows the man that I wonder which restaurant hired him',
    kor: '은지는 작년에 어느 식당이 그를 채용했는지 내가 궁금해하는 남자를 안다.',
    zho: '李娜認識那個我好奇哪家餐廳去年雇用他的男人。',
    cond: 'cond6',
    ...dr13
};

var dr14 = {
    type: 'critical',
    item: 'item14',
    coda: ' at the end of the year.',
    time: ' last year.',
    text_center: 'hired',
    choices1: jsPsych.randomization.shuffle(['Bill', 'I', 'the woman', 'the business']),
    correct1: 'the business',
    choices2: jsPsych.randomization.shuffle(['Bill', 'me', 'the woman', 'the business']),
    correct2: 'the woman'
};
var dr14_cond1 = {
    eng: 'I think Bill knows the woman that this business hired',
    kor: '내가 생각하기에 우진이는 작년에 이 사업장이 채용한 여자를 안다.',
    zho: '我覺得吳強認識那個這家企業去年雇用的女人。',
    cond: 'cond1',
    ...dr14
};
var dr14_cond2 = {
    eng: 'Bill knows the woman that I think this business hired',
    kor: '우진이는 작년에 이 사업장이 채용했다고 내가 생각하는 여자를 안다.',
    zho: '吳強認識那個我覺得這家企業去年雇用的女人。',
    cond: 'cond2',
    ...dr14
};
var dr14_cond3 = {
    eng: 'Bill knows the woman that I wonder which business hired',
    kor: '우진이는 작년에 어느 사업장이 채용했는지 내가 궁금해하는 여자를 안다.',
    zho: '吳強認識那個我好奇哪家企業去年雇用的女人。',
    cond: 'cond3',
    ...dr14
};
var dr14_cond4 = {
    eng: 'I think Bill knows the woman that this business hired her',
    kor: '내가 생각하기에 우진이는 작년에 이 사업장이 그녀를 채용한 여자를 안다.',
    zho: '我覺得吳強認識那個這家企業去年雇用她的女人。',
    cond: 'cond4',
    ...dr14
};
var dr14_cond5 = {
    eng: 'Bill knows the woman that I think this business hired her',
    kor: '우진이는 작년에 이 사업장이 그녀를 채용했다고 내가 생각하는 여자를 안다.',
    zho: '吳強認識那個我覺得這家企業去年雇用她的女人。',
    cond: 'cond5',
    ...dr14
};
var dr14_cond6 = {
    eng: 'Bill knows the woman that I wonder which business hired her',
    kor: '우진이는 작년에 어느 사업장이 그녀를 채용했는지 내가 궁금해하는 여자를 안다.',
    zho: '吳強認識那個我好奇哪家企業去年雇用她的女人。',
    cond: 'cond6',
    ...dr14
};

var dr15 = {
    type: 'critical',
    item: 'item15',
    coda: ' at the end of the year.',
    time: ' last year.',
    text_center: 'punished',
    choices1: jsPsych.randomization.shuffle(['Tina', 'I', 'the man', 'the soldiers']),
    correct1: 'the soldiers',
    choices2: jsPsych.randomization.shuffle(['Tina', 'me', 'the man', 'the soldiers']),
    correct2: 'the man'
};
var dr15_cond1 = {
    eng: 'I think Tina knows the man that these soldiers punished',
    kor: '내가 생각하기에 수아는 작년에 이 군인들이 처벌한 남자를 안다.',
    zho: '我覺得趙悅認識那個這些士兵去年懲罰的男人。',
    cond: 'cond1',
    ...dr15
};
var dr15_cond2 = {
    eng: 'Tina knows the man that I think these soldiers punished',
    kor: '수아는 작년에 이 군인들이 처벌했다고 내가 생각하는 남자를 안다.',
    zho: '趙悅認識那個我覺得這些士兵去年懲罰的男人。',
    cond: 'cond2',
    ...dr15
};
var dr15_cond3 = {
    eng: 'Tina knows the man that I wonder which soldiers punished',
    kor: '수아는 작년에 어느 군인들이 처벌했는지 내가 궁금해하는 남자를 안다.',
    zho: '趙悅認識那個我好奇哪些士兵去年懲罰的男人。',
    cond: 'cond3',
    ...dr15
};
var dr15_cond4 = {
    eng: 'I think Tina knows the man that these soldiers punished him',
    kor: '내가 생각하기에 수아는 작년에 이 군인들이 그를 처벌한 남자를 안다.',
    zho: '我覺得趙悅認識那個這些士兵去年懲罰他的男人。',
    cond: 'cond4',
    ...dr15
};
var dr15_cond5 = {
    eng: 'Tina knows the man that I think these soldiers punished him',
    kor: '수아는 작년에 이 군인들이 그를 처벌했다고 내가 생각하는 남자를 안다.',
    zho: '趙悅認識那個我覺得這些士兵去年懲罰他的男人。',
    cond: 'cond5',
    ...dr15
};
var dr15_cond6 = {
    eng: 'Tina knows the man that I wonder which soldiers punished him',
    kor: '수아는 작년에 어느 군인들이 그를 처벌했는지 내가 궁금해하는 남자를 안다.',
    zho: '趙悅認識那個我好奇哪些士兵去年懲罰他的男人。',
    cond: 'cond6',
    ...dr15
};

var dr16 = {
    type: 'critical',
    item: 'item16',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_center: 'punished',
    choices1: jsPsych.randomization.shuffle(['Mike', 'I', 'the woman', 'the agency']),
    correct1: 'the agency',
    choices2: jsPsych.randomization.shuffle(['Mike', 'me', 'the woman', 'the agency']),
    correct2: 'the woman'
};
var dr16_cond1 = {
    eng: 'I think Mike knows the woman that this agency punished',
    kor: '내가 생각하기에 서준이는 작년에 이 기관이 처벌한 여자를 안다.',
    zho: '我覺得楊毅認識那個這個機構去年懲罰的女人。',
    cond: 'cond1',
    ...dr16
};
var dr16_cond2 = {
    eng: 'Mike knows the woman that I think this agency punished',
    kor: '서준이는 작년에 이 기관이 처벌했다고 내가 생각하는 여자를 안다.',
    zho: '楊毅認識那個我覺得這個機構去年懲罰的女人。',
    cond: 'cond2',
    ...dr16
};
var dr16_cond3 = {
    eng: 'Mike knows the woman that I wonder which agency punished',
    kor: '서준이는 작년에 어느 기관이 처벌했는지 내가 궁금해하는 여자를 안다.',
    zho: '楊毅認識那個我好奇哪個機構去年懲罰的女人。',
    cond: 'cond3',
    ...dr16
};
var dr16_cond4 = {
    eng: 'I think Mike knows the woman that this agency punished her',
    kor: '내가 생각하기에 서준이는 작년에 이 기관이 그녀를 처벌한 여자를 안다.',
    zho: '我覺得楊毅認識那個這個機構去年懲罰她的女人。',
    cond: 'cond4',
    ...dr16
};
var dr16_cond5 = {
    eng: 'Mike knows the woman that I think this agency punished her',
    kor: '서준이는 작년에 이 기관이 그녀를 처벌했다고 내가 생각하는 여자를 안다.',
    zho: '楊毅認識那個我覺得這個機構去年懲罰她的女人。',
    cond: 'cond5',
    ...dr16
};
var dr16_cond6 = {
    eng: 'Mike knows the woman that I wonder which agency punished her',
    kor: '서준이는 작년에 어느 기관이 그녀를 처벌했는지 내가 궁금해하는 여자를 안다.',
    zho: '楊毅認識那個我好奇哪個機構去年懲罰她的女人。',
    cond: 'cond6',
    ...dr16
};

var dr17 = {
    type: 'critical',
    item: 'item17',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_center: 'questioned',
    choices1: jsPsych.randomization.shuffle(['Judy', 'I', 'the man', 'the lawyers']),
    correct1: 'the lawyers',
    choices2: jsPsych.randomization.shuffle(['Judy', 'me', 'the man', 'the lawyers']),
    correct2: 'the man'
};
var dr17_cond1 = {
    eng: 'I think Judy knows the man that these lawyers questioned',
    kor: '내가 생각하기에 지은이는 지난주에 이 변호사들이 심문한 남자를 안다.',
    zho: '我覺得周靜認識那個這些律師上個星期審問的男人。',
    cond: 'cond1',
    ...dr17
};
var dr17_cond2 = {
    eng: 'Judy knows the man that I think these lawyers questioned',
    kor: '지은이는 지난주에 이 변호사들이 심문했다고 내가 생각하는 남자를 안다.',
    zho: '周靜認識那個我覺得這些律師上個星期審問的男人。',
    cond: 'cond2',
    ...dr17
};
var dr17_cond3 = {
    eng: 'Judy knows the man that I wonder which lawyers questioned',
    kor: '지은이는 지난주에 어느 변호사들이 심문했는지 내가 궁금해하는 남자를 안다.',
    zho: '周靜認識那個我好奇哪些律師上個星期審問的男人。',
    cond: 'cond3',
    ...dr17
};
var dr17_cond4 = {
    eng: 'I think Judy knows the man that these lawyers questioned him',
    kor: '내가 생각하기에 지은이는 지난주에 이 변호사들이 그를 심문한 남자를 안다.',
    zho: '我覺得周靜認識那個這些律師上個星期審問他的男人。',
    cond: 'cond4',
    ...dr17
};
var dr17_cond5 = {
    eng: 'Judy knows the man that I think these lawyers questioned him',
    kor: '지은이는 지난주에 이 변호사들이 그를 심문했다고 내가 생각하는 남자를 안다.',
    zho: '周靜認識那個我覺得這些律師上個星期審問他的男人。',
    cond: 'cond5',
    ...dr17
};
var dr17_cond6 = {
    eng: 'Judy knows the man that I wonder which lawyers questioned him',
    kor: '지은이는 지난주에 어느 변호사들이 그를 심문했는지 내가 궁금해하는 남자를 안다.',
    zho: '周靜認識那個我好奇哪些律師上個星期審問他的男人。',
    cond: 'cond6',
    ...dr17
};

var dr18 = {
    type: 'critical',
    item: 'item18',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_center: 'questioned',
    choices1: jsPsych.randomization.shuffle(['Luke', 'I', 'the woman', 'the soldiers']),
    correct1: 'the soldiers',
    choices2: jsPsych.randomization.shuffle(['Luke', 'me', 'the woman', 'the soldiers']),
    correct2: 'the woman'
};
var dr18_cond1 = {
    eng: 'I think Luke knows the woman that these soldiers questioned',
    kor: '내가 생각하기에 현우는 지난주에 이 군인들이 심문한 여자를 안다.',
    zho: '我覺得陳傑認識那個這些士兵上個星期審問的女人。',
    cond: 'cond1',
    ...dr18
};
var dr18_cond2 = {
    eng: 'Luke knows the woman that I think these soldiers questioned',
    kor: '현우는 지난주에 이 군인들이 심문했다고 내가 생각하는 여자를 안다.',
    zho: '陳傑認識那個我覺得這些士兵上個星期審問的女人。',
    cond: 'cond2',
    ...dr18
};
var dr18_cond3 = {
    eng: 'Luke knows the woman that I wonder which soldiers questioned',
    kor: '현우는 지난주에 어느 군인들이 심문했는지 내가 궁금해하는 여자를 안다.',
    zho: '陳傑認識那個我好奇哪些士兵上個星期審問的女人。',
    cond: 'cond3',
    ...dr18
};
var dr18_cond4 = {
    eng: 'I think Luke knows the woman that these soldiers questioned her',
    kor: '내가 생각하기에 현우는 지난주에 이 군인들이 그녀를 심문한 여자를 안다.',
    zho: '我覺得陳傑認識那個這些士兵上個星期審問她的女人。',
    cond: 'cond4',
    ...dr18
};
var dr18_cond5 = {
    eng: 'Luke knows the woman that I think these soldiers questioned her',
    kor: '현우는 지난주에 이 군인들이 그녀를 심문했다고 내가 생각하는 여자를 안다.',
    zho: '陳傑認識那個我覺得這些士兵上個星期審問她的女人。',
    cond: 'cond5',
    ...dr18
};
var dr18_cond6 = {
    eng: 'Luke knows the woman that I wonder which soldiers questioned her',
    kor: '현우는 지난주에 어느 군인들이 그녀를 심문했는지 내가 궁금해하는 여자를 안다.',
    zho: '陳傑認識那個我好奇哪些士兵上個星期審問她的女人。',
    cond: 'cond6',
    ...dr18
};

var dr19 = {
    type: 'critical',
    item: 'item19',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_center: 'recruited',
    choices1: jsPsych.randomization.shuffle(['Anna', 'I', 'the man', 'the agency']),
    correct1: 'the agency',
    choices2: jsPsych.randomization.shuffle(['Anna', 'me', 'the man', 'the agency']),
    correct2: 'the man'
};
var dr19_cond1 = {
    eng: 'I think Anna knows the man that this agency recruited',
    kor: '내가 생각하기에 하은이는 지난달에 이 기관이 뽑은 남자를 안다.',
    zho: '我覺得王芳認識那個這個機構上個月招聘的男人。',
    cond: 'cond1',
    ...dr19
};
var dr19_cond2 = {
    eng: 'Anna knows the man that I think this agency recruited',
    kor: '하은이는 지난달에 이 기관이 뽑았다고 내가 생각하는 남자를 안다.',
    zho: '王芳認識那個我覺得這個機構上個月招聘的男人。',
    cond: 'cond2',
    ...dr19
};
var dr19_cond3 = {
    eng: 'Anna knows the man that I wonder which agency recruited',
    kor: '하은이는 지난달에 어느 기관이 뽑았는지 내가 궁금해하는 남자를 안다.',
    zho: '王芳認識那個我好奇哪個機構上個月招聘的男人。',
    cond: 'cond3',
    ...dr19
};
var dr19_cond4 = {
    eng: 'I think Anna knows the man that this agency recruited him',
    kor: '내가 생각하기에 하은이는 지난달에 이 기관이 그를 뽑은 남자를 안다.',
    zho: '我覺得王芳認識那個這個機構上個月招聘他的男人。',
    cond: 'cond4',
    ...dr19
};
var dr19_cond5 = {
    eng: 'Anna knows the man that I think this agency recruited him',
    kor: '하은이는 지난달에 이 기관이 그를 뽑았다고 내가 생각하는 남자를 안다.',
    zho: '王芳認識那個我覺得這個機構上個月招聘他的男人。',
    cond: 'cond5',
    ...dr19
};
var dr19_cond6 = {
    eng: 'Anna knows the man that I wonder which agency recruited him',
    kor: '하은이는 지난달에 어느 기관이 그를 뽑았는지 내가 궁금해하는 남자를 안다.',
    zho: '王芳認識那個我好奇哪個機構上個月招聘他的男人。',
    cond: 'cond6',
    ...dr19
};

var dr20 = {
    type: 'critical',
    item: 'item20',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_center: 'recruited',
    choices1: jsPsych.randomization.shuffle(['Dave', 'I', 'the woman', 'the firm']),
    correct1: 'the firm',
    choices2: jsPsych.randomization.shuffle(['Dave', 'me', 'the woman', 'the firm']),
    correct2: 'the woman'
};
var dr20_cond1 = {
    eng: 'I think Dave knows the woman that this firm recruited',
    kor: '내가 생각하기에 도윤이는 지난달에 이 로펌이 뽑은 여자를 안다.',
    zho: '我覺得劉浩認識那個這家律師事務所上個月招聘的女人。',
    cond: 'cond1',
    ...dr20
};
var dr20_cond2 = {
    eng: 'Dave knows the woman that I think this firm recruited',
    kor: '도윤이는 지난달에 이 로펌이 뽑았다고 내가 생각하는 여자를 안다.',
    zho: '劉浩認識那個我覺得這家律師事務所上個月招聘的女人。',
    cond: 'cond2',
    ...dr20
};
var dr20_cond3 = {
    eng: 'Dave knows the woman that I wonder which firm recruited',
    kor: '도윤이는 지난달에 어느 로펌이 뽑았는지 내가 궁금해하는 여자를 안다.',
    zho: '劉浩認識那個我好奇哪家律師事務所上個月招聘的女人。',
    cond: 'cond3',
    ...dr20
};
var dr20_cond4 = {
    eng: 'I think Dave knows the woman that this firm recruited her',
    kor: '내가 생각하기에 도윤이는 지난달에 이 로펌이 그녀를 뽑은 여자를 안다.',
    zho: '我覺得劉浩認識那個這家律師事務所上個月招聘她的女人。',
    cond: 'cond4',
    ...dr20
};
var dr20_cond5 = {
    eng: 'Dave knows the woman that I think this firm recruited her',
    kor: '도윤이는 지난달에 이 로펌이 그녀를 뽑았다고 내가 생각하는 여자를 안다.',
    zho: '劉浩認識那個我覺得這家律師事務所上個月招聘她的女人。',
    cond: 'cond5',
    ...dr20
};
var dr20_cond6 = {
    eng: 'Dave knows the woman that I wonder which firm recruited her',
    kor: '도윤이는 지난달에 어느 로펌이 그녀를 뽑았는지 내가 궁금해하는 여자를 안다.',
    zho: '劉浩認識那個我好奇哪家律師事務所上個月招聘她的女人。',
    cond: 'cond6',
    ...dr20
};

var dr21 = {
    type: 'critical',
    item: 'item21',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_center: 'released',
    choices1: jsPsych.randomization.shuffle(['Mary', 'I', 'the man', 'the guards']),
    correct1: 'the guards',
    choices2: jsPsych.randomization.shuffle(['Mary', 'me', 'the man', 'the guards']),
    correct2: 'the man'
};
var dr21_cond1 = {
    eng: 'I think Mary knows the man that these guards released',
    kor: '내가 생각하기에 민지는 지난달에 이 경비원들이 석방시킨 남자를 안다.',
    zho: '我覺得黃麗認識那個這些獄卒上個月放走的男人。',
    cond: 'cond1',
    ...dr21
};
var dr21_cond2 = {
    eng: 'Mary knows the man that I think these guards released',
    kor: '민지는 지난달에 이 경비원들이 석방시켰다고 내가 생각하는 남자를 안다.',
    zho: '黃麗認識那個我覺得這些獄卒上個月放走的男人。',
    cond: 'cond2',
    ...dr21
};
var dr21_cond3 = {
    eng: 'Mary knows the man that I wonder which guards released',
    kor: '민지는 지난달에 어느 경비원들이 석방시켰는지 내가 궁금해하는 남자를 안다.',
    zho: '黃麗認識那個我好奇哪些獄卒上個月放走的男人。',
    cond: 'cond3',
    ...dr21
};
var dr21_cond4 = {
    eng: 'I think Mary knows the man that these guards released him',
    kor: '내가 생각하기에 민지는 지난달에 이 경비원들이 그를 석방시킨 남자를 안다.',
    zho: '我覺得黃麗認識那個這些獄卒上個月放走他的男人。',
    cond: 'cond4',
    ...dr21
};
var dr21_cond5 = {
    eng: 'Mary knows the man that I think these guards released him',
    kor: '민지는 지난달에 이 경비원들이 그를 석방시켰다고 내가 생각하는 남자를 안다.',
    zho: '黃麗認識那個我覺得這些獄卒上個月放走他的男人。',
    cond: 'cond5',
    ...dr21
};
var dr21_cond6 = {
    eng: 'Mary knows the man that I wonder which guards released him',
    kor: '민지는 지난달에 어느 경비원들이 그를 석방시켰는지 내가 궁금해하는 남자를 안다.',
    zho: '黃麗認識那個我好奇哪些獄卒上個月放走他的男人。',
    cond: 'cond6',
    ...dr21
};

var dr22 = {
    type: 'critical',
    item: 'item22',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_center: 'released',
    choices1: jsPsych.randomization.shuffle(['John', 'I', 'the woman', 'the officials']),
    correct1: 'the officials',
    choices2: jsPsych.randomization.shuffle(['John', 'me', 'the woman', 'the officials']),
    correct2: 'the woman'
};
var dr22_cond1 = {
    eng: 'I think John knows the woman that these officials released',
    kor: '내가 생각하기에 철수는 지난달에 이 관계자들이 석방시킨 여자를 안다.',
    zho: '我覺得張偉認識那個官員上個月放走的女人。',
    cond: 'cond1',
    ...dr22
};
var dr22_cond2 = {
    eng: 'John knows the woman that I think these officials released',
    kor: '철수는 지난달에 이 관계자들이 석방시켰다고 내가 생각하는 여자를 안다.',
    zho: '張偉認識那個我覺得這些官員上個月放走的女人。',
    cond: 'cond2',
    ...dr22
};
var dr22_cond3 = {
    eng: 'John knows the woman that I wonder which officials released',
    kor: '철수는 지난달에 어느 관계자들이 석방시켰는지 내가 궁금해하는 여자를 안다.',
    zho: '張偉認識那個我好奇哪些官員上個月放走的女人。',
    cond: 'cond3',
    ...dr22
};
var dr22_cond4 = {
    eng: 'I think John knows the woman that these officials released her',
    kor: '내가 생각하기에 철수는 지난달에 이 관계자들이 그녀를 석방시킨 여자를 안다.',
    zho: '我覺得張偉認識那個這些官員上個月放走她的女人。',
    cond: 'cond4',
    ...dr22
};
var dr22_cond5 = {
    eng: 'John knows the woman that I think these officials released her',
    kor: '철수는 지난달에 이 관계자들이 그녀를 석방시켰다고 내가 생각하는 여자를 안다.',
    zho: '張偉認識那個我覺得這些官員上個月放走她的女人。',
    cond: 'cond5',
    ...dr22
};
var dr22_cond6 = {
    eng: 'John knows the woman that I wonder which officials released her',
    kor: '철수는 지난달에 어느 관계자들이 그녀를 석방시켰는지 내가 궁금해하는 여자를 안다.',
    zho: '張偉認識那個我好奇哪些官員上個月放走她的女人。',
    cond: 'cond6',
    ...dr22
};

var dr23 = {
    type: 'critical',
    item: 'item23',
    coda: ' at the end of the night.',
    time: ' last night.',
    text_center: 'rescued',
    choices1: jsPsych.randomization.shuffle(['Lisa', 'I', 'the man', 'the guards']),
    correct1: 'the guards',
    choices2: jsPsych.randomization.shuffle(['Lisa', 'me', 'the man', 'the guards']),
    correct2: 'the man'
};
var dr23_cond1 = {
    eng: 'I think Lisa knows the man that these guards rescued',
    kor: '내가 생각하기에 은지는 어젯밤에 이 경비원들이 구조한 남자를 안다.',
    zho: '我覺得李娜認識那個這些獄卒昨晚救出的男人。',
    cond: 'cond1',
    ...dr23
};
var dr23_cond2 = {
    eng: 'Lisa knows the man that I think these guards rescued',
    kor: '은지는 어젯밤에 이 경비원들이 구조했다고 내가 생각하는 남자를 안다.',
    zho: '李娜認識那個我覺得這些獄卒昨晚救出的男人。',
    cond: 'cond2',
    ...dr23
};
var dr23_cond3 = {
    eng: 'Lisa knows the man that I wonder which guards rescued',
    kor: '은지는 어젯밤에 어느 경비원들이 구조했는지 내가 궁금해하는 남자를 안다.',
    zho: '李娜認識那個我好奇哪些獄卒昨晚救出的男人。',
    cond: 'cond3',
    ...dr23
};
var dr23_cond4 = {
    eng: 'I think Lisa knows the man that these guards rescued him',
    kor: '내가 생각하기에 은지는 어젯밤에 이 경비원들이 그를 구조한 남자를 안다.',
    zho: '我覺得李娜認識那個這些獄卒昨晚救出他的男人。',
    cond: 'cond4',
    ...dr23
};
var dr23_cond5 = {
    eng: 'Lisa knows the man that I think these guards rescued him',
    kor: '은지는 어젯밤에 이 경비원들이 그를 구조했다고 내가 생각하는 남자를 안다.',
    zho: '李娜認識那個我覺得這些獄卒昨晚救出他的男人。',
    cond: 'cond5',
    ...dr23
};
var dr23_cond6 = {
    eng: 'Lisa knows the man that I wonder which guards rescued him',
    kor: '은지는 어젯밤에 어느 경비원들이 그를 구조했는지 내가 궁금해하는 남자를 안다.',
    zho: '李娜認識那個我好奇哪些獄卒昨晚救出他的男人。',
    cond: 'cond6',
    ...dr23
};

var dr24 = {
    type: 'critical',
    item: 'item24',
    coda: ' at the beginning of the night.',
    time: ' last night.',
    text_center: 'rescued',
    choices1: jsPsych.randomization.shuffle(['Bill', 'I', 'the woman', 'the officers']),
    correct1: 'the officers',
    choices2: jsPsych.randomization.shuffle(['Bill', 'me', 'the woman', 'the officers']),
    correct2: 'the woman'
};
var dr24_cond1 = {
    eng: 'I think Bill knows the woman that these officers rescued',
    kor: '내가 생각하기에 우진이는 어젯밤에 이 경찰들이 구조한 여자를 안다.',
    zho: '我覺得吳強認識那個這些警官昨晚救出的女人。',
    cond: 'cond1',
    ...dr24
};
var dr24_cond2 = {
    eng: 'Bill knows the woman that I think these officers rescued',
    kor: '우진이는 어젯밤에 이 경찰들이 구조했다고 내가 생각하는 여자를 안다.',
    zho: '吳強認識那個我覺得這些警官昨晚救出的女人。',
    cond: 'cond2',
    ...dr24
};
var dr24_cond3 = {
    eng: 'Bill knows the woman that I wonder which officers rescued',
    kor: '우진이는 어젯밤에 어느 경찰들이 구조했는지 내가 궁금해하는 여자를 안다.',
    zho: '吳強認識那個我好奇哪些警官昨晚救出的女人。',
    cond: 'cond3',
    ...dr24
};
var dr24_cond4 = {
    eng: 'I think Bill knows the woman that these officers rescued her',
    kor: '내가 생각하기에 우진이는 어젯밤에 이 경찰들이 그녀를 구조한 여자를 안다.',
    zho: '我覺得吳強認識那個這些警官昨晚救出她的女人。',
    cond: 'cond4',
    ...dr24
};
var dr24_cond5 = {
    eng: 'Bill knows the woman that I think these officers rescued her',
    kor: '우진이는 어젯밤에 이 경찰들이 그녀를 구조했다고 내가 생각하는 여자를 안다.',
    zho: '吳強認識那個我覺得這些警官昨晚救出她的女人。',
    cond: 'cond5',
    ...dr24
};
var dr24_cond6 = {
    eng: 'Bill knows the woman that I wonder which officers rescued her',
    kor: '우진이는 어젯밤에 어느 경찰들이 그녀를 구조했는지 내가 궁금해하는 여자를 안다.',
    zho: '吳強認識那個我好奇哪些警官昨晚救出她的女人。',
    cond: 'cond6',
    ...dr24
};

var dr25 = {
    type: 'critical',
    item: 'item25',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_center: 'treated',
    choices1: jsPsych.randomization.shuffle(['Tina', 'I', 'the man', 'the doctors']),
    correct1: 'the doctors',
    choices2: jsPsych.randomization.shuffle(['Tina', 'me', 'the man', 'the doctors']),
    correct2: 'the man'
};
var dr25_cond1 = {
    eng: 'I think Tina knows the man that these doctors treated',
    kor: '내가 생각하기에 수아는 지난주에 이 의사들이 치료한 남자를 안다.',
    zho: '我覺得趙悅認識那個這些醫生上個星期治療的男人。',
    cond: 'cond1',
    ...dr25
};
var dr25_cond2 = {
    eng: 'Tina knows the man that I think these doctors treated',
    kor: '수아는 지난주에 이 의사들이 치료했다고 내가 생각하는 남자를 안다.',
    zho: '趙悅認識那個我覺得這些醫生上個星期治療的男人。',
    cond: 'cond2',
    ...dr25
};
var dr25_cond3 = {
    eng: 'Tina knows the man that I wonder which doctors treated',
    kor: '수아는 지난주에 어느 의사들이 치료했는지 내가 궁금해하는 남자를 안다.',
    zho: '趙悅認識那個我好奇哪些醫生上個星期治療的男人。',
    cond: 'cond3',
    ...dr25
};
var dr25_cond4 = {
    eng: 'I think Tina knows the man that these doctors treated him',
    kor: '내가 생각하기에 수아는 지난주에 이 의사들이 그를 치료한 남자를 안다.',
    zho: '我覺得趙悅認識那個這些醫生上個星期治療他的男人。',
    cond: 'cond4',
    ...dr25
};
var dr25_cond5 = {
    eng: 'Tina knows the man that I think these doctors treated him',
    kor: '수아는 지난주에 이 의사들이 그를 치료했다고 내가 생각하는 남자를 안다.',
    zho: '趙悅認識那個我覺得這些醫生上個星期治療他的男人。',
    cond: 'cond5',
    ...dr25
};
var dr25_cond6 = {
    eng: 'Tina knows the man that I wonder which doctors treated him',
    kor: '수아는 지난주에 어느 의사들이 그를 치료했는지 내가 궁금해하는 남자를 안다.',
    zho: '趙悅認識那個我好奇哪些醫生上個星期治療他的男人。',
    cond: 'cond6',
    ...dr25
};

var dr26 = {
    type: 'critical',
    item: 'item26',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_center: 'treated',
    choices1: jsPsych.randomization.shuffle(['Mike', 'I', 'the woman', 'the nurses']),
    correct1: 'the nurses',
    choices2: jsPsych.randomization.shuffle(['Mike', 'me', 'the woman', 'the nurses']),
    correct2: 'the woman'
};
var dr26_cond1 = {
    eng: 'I think Mike knows the woman that these nurses treated',
    kor: '내가 생각하기에 서준이는 지난주에 이 간호사들이 치료한 여자를 안다.',
    zho: '我覺得楊毅認識那個這些護理師上個星期治療的女人。',
    cond: 'cond1',
    ...dr26
};
var dr26_cond2 = {
    eng: 'Mike knows the woman that I think these nurses treated',
    kor: '서준이는 지난주에 이 간호사들이 치료했다고 내가 생각하는 여자를 안다.',
    zho: '楊毅認識那個我覺得這些護理師上個星期治療的女人。',
    cond: 'cond2',
    ...dr26
};
var dr26_cond3 = {
    eng: 'Mike knows the woman that I wonder which nurses treated',
    kor: '서준이는 지난주에 어느 간호사들이 치료했는지 내가 궁금해하는 여자를 안다.',
    zho: '楊毅認識那個我好奇哪些護理師上個星期治療的女人。',
    cond: 'cond3',
    ...dr26
};
var dr26_cond4 = {
    eng: 'I think Mike knows the woman that these nurses treated her',
    kor: '내가 생각하기에 서준이는 지난주에 이 간호사들이 그녀를 치료한 여자를 안다.',
    zho: '我覺得楊毅認識那個這些護理師上個星期治療她的女人。',
    cond: 'cond4',
    ...dr26
};
var dr26_cond5 = {
    eng: 'Mike knows the woman that I think these nurses treated her',
    kor: '서준이는 지난주에 이 간호사들이 그녀를 치료했다고 내가 생각하는 여자를 안다.',
    zho: '楊毅認識那個我覺得這些護理師上個星期治療她的女人。',
    cond: 'cond5',
    ...dr26
};
var dr26_cond6 = {
    eng: 'Mike knows the woman that I wonder which nurses treated her',
    kor: '서준이는 지난주에 어느 간호사들이 그녀를 치료했는지 내가 궁금해하는 여자를 안다.',
    zho: '楊毅認識那個我好奇哪些護理師上個星期治療她的女人。',
    cond: 'cond6',
    ...dr26
};

var dr27 = {
    type: 'critical',
    item: 'item27',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_center: 'welcomed',
    choices1: jsPsych.randomization.shuffle(['Judy', 'I', 'the man', 'the committee']),
    correct1: 'the committee',
    choices2: jsPsych.randomization.shuffle(['Judy', 'me', 'the man', 'the committee']),
    correct2: 'the man'
};
var dr27_cond1 = {
    eng: 'I think Judy knows the man that this committee welcomed',
    kor: '내가 생각하기에 지은이는 지난달에 이 위원회가 환영한 남자를 안다.',
    zho: '我覺得周靜認識那個這個委員會上個月歡迎的男人。',
    cond: 'cond1',
    ...dr27
};
var dr27_cond2 = {
    eng: 'Judy knows the man that I think this committee welcomed',
    kor: '지은이는 지난달에 이 위원회가 환영했다고 내가 생각하는 남자를 안다.',
    zho: '周靜認識那個我覺得這個委員會上個月歡迎的男人。',
    cond: 'cond2',
    ...dr27
};
var dr27_cond3 = {
    eng: 'Judy knows the man that I wonder which committee welcomed',
    kor: '지은이는 지난달에 어느 위원회가 환영했는지 내가 궁금해하는 남자를 안다.',
    zho: '周靜認識那個我好奇哪個委員會上個月歡迎的男人。',
    cond: 'cond3',
    ...dr27
};
var dr27_cond4 = {
    eng: 'I think Judy knows the man that this committee welcomed him',
    kor: '내가 생각하기에 지은이는 지난달에 이 위원회가 그를 환영한 남자를 안다.',
    zho: '我覺得周靜認識那個這個委員會上個月歡迎他的男人。',
    cond: 'cond4',
    ...dr27
};
var dr27_cond5 = {
    eng: 'Judy knows the man that I think this committee welcomed him',
    kor: '지은이는 지난달에 이 위원회가 그를 환영했다고 내가 생각하는 남자를 안다.',
    zho: '周靜認識那個我覺得這個委員會上個月歡迎他的男人。',
    cond: 'cond5',
    ...dr27
};
var dr27_cond6 = {
    eng: 'Judy knows the man that I wonder which committee welcomed him',
    kor: '지은이는 지난달에 어느 위원회가 그를 환영했는지 내가 궁금해하는 남자를 안다.',
    zho: '周靜認識那個我好奇哪個委員會上個月歡迎他的男人。',
    cond: 'cond6',
    ...dr27
};

var dr28 = {
    type: 'critical',
    item: 'item28',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_center: 'welcomed',
    choices1: jsPsych.randomization.shuffle(['Luke', 'I', 'the woman', 'the organization']),
    correct1: 'the organization',
    choices2: jsPsych.randomization.shuffle(['Luke', 'me', 'the woman', 'the organization']),
    correct2: 'the woman'
};
var dr28_cond1 = {
    eng: 'I think Luke knows the woman that this organization welcomed',
    kor: '내가 생각하기에 현우는 지난달에 이 단체가 환영한 여자를 안다.',
    zho: '我覺得陳傑認識那個這個團體上個月歡迎的女人。',
    cond: 'cond1',
    ...dr28
};
var dr28_cond2 = {
    eng: 'Luke knows the woman that I think this organization welcomed',
    kor: '현우는 지난달에 이 단체가 환영했다고 내가 생각하는 여자를 안다.',
    zho: '陳傑認識那個我覺得這個團體上個月歡迎的女人。',
    cond: 'cond2',
    ...dr28
};
var dr28_cond3 = {
    eng: 'Luke knows the woman that I wonder which organization welcomed',
    kor: '현우는 지난달에 어느 단체가 환영했는지 내가 궁금해하는 여자를 안다.',
    zho: '陳傑認識那個我好奇哪個團體上個月歡迎的女人。',
    cond: 'cond3',
    ...dr28
};
var dr28_cond4 = {
    eng: 'I think Luke knows the woman that this organization welcomed her',
    kor: '내가 생각하기에 현우는 지난달에 이 단체가 그녀를 환영한 여자를 안다.',
    zho: '我覺得陳傑認識那個這個團體上個月歡迎她的女人。',
    cond: 'cond4',
    ...dr28
};
var dr28_cond5 = {
    eng: 'Luke knows the woman that I think this organization welcomed her',
    kor: '현우는 지난달에 이 단체가 그녀를 환영했다고 내가 생각하는 여자를 안다.',
    zho: '陳傑認識那個我覺得這個團體上個月歡迎她的女人。',
    cond: 'cond5',
    ...dr28
};
var dr28_cond6 = {
    eng: 'Luke knows the woman that I wonder which organization welcomed her',
    kor: '현우는 지난달에 어느 단체가 그녀를 환영했는지 내가 궁금해하는 여자를 안다.',
    zho: '陳傑認識那個我好奇哪個團體上個月歡迎她的女人。',
    cond: 'cond6',
    ...dr28
};

var dr29 = {
    type: 'critical',
    item: 'item29',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_center: 'interviewed',
    choices1: jsPsych.randomization.shuffle(['Anna', 'I', 'the man', 'the committee']),
    correct1: 'the committee',
    choices2: jsPsych.randomization.shuffle(['Anna', 'me', 'the man', 'the committee']),
    correct2: 'the man'
};
var dr29_cond1 = {
    eng: 'I think Anna knows the man that this committee interviewed',
    kor: '내가 생각하기에 하은이는 지난주에 이 위원회가 인터뷰한 남자를 안다.',
    zho: '我覺得王芳認識那個這個委員會上個星期採訪的男人。',
    cond: 'cond1',
    ...dr29
};
var dr29_cond2 = {
    eng: 'Anna knows the man that I think this committee interviewed',
    kor: '하은이는 지난주에 이 위원회가 인터뷰했다고 내가 생각하는 남자를 안다.',
    zho: '王芳認識那個我覺得這個委員會上個星期採訪的男人。',
    cond: 'cond2',
    ...dr29
};
var dr29_cond3 = {
    eng: 'Anna knows the man that I wonder which committee interviewed',
    kor: '하은이는 지난주에 어느 위원회가 인터뷰했는지 내가 궁금해하는 남자를 안다.',
    zho: '王芳認識那個我好奇哪個委員會上個星期採訪的男人。',
    cond: 'cond3',
    ...dr29
};
var dr29_cond4 = {
    eng: 'I think Anna knows the man that this committee interviewed him',
    kor: '내가 생각하기에 하은이는 지난주에 이 위원회가 그를 인터뷰한 남자를 안다.',
    zho: '我覺得王芳認識那個這個委員會上個星期採訪他的男人',
    cond: 'cond4',
    ...dr29
};
var dr29_cond5 = {
    eng: 'Anna knows the man that I think this committee interviewed him',
    kor: '하은이는 지난주에 이 위원회가 그를 인터뷰했다고 내가 생각하는 남자를 안다.',
    zho: '王芳認識那個我覺得這個委員會上個星期採訪他的男人。',
    cond: 'cond5',
    ...dr29
};
var dr29_cond6 = {
    eng: 'Anna knows the man that I wonder which committee interviewed him',
    kor: '하은이는 지난주에 어느 위원회가 그를 인터뷰했는지 내가 궁금해하는 남자를 안다.',
    zho: '王芳認識那個我好奇哪個委員會上個星期採訪他的男人。',
    cond: 'cond6',
    ...dr29
};

var dr30 = {
    type: 'critical',
    item: 'item30',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_center: 'interviewed',
    choices1: jsPsych.randomization.shuffle(['Dave', 'I', 'the woman', 'the organization']),
    correct1: 'the organization',
    choices2: jsPsych.randomization.shuffle(['Dave', 'me', 'the woman', 'the organization']),
    correct2: 'the woman'
};
var dr30_cond1 = {
    eng: 'I think Dave knows the woman that this organization interviewed',
    kor: '내가 생각하기에 도윤이는 지난주에 이 단체가 인터뷰한 여자를 안다.',
    zho: '我覺得劉浩認識那個這個團體上個星期採訪的女人。',
    cond: 'cond1',
    ...dr30
};
var dr30_cond2 = {
    eng: 'Dave knows the woman that I think this organization interviewed',
    kor: '도윤이는 지난주에 이 단체가 인터뷰했다고 내가 생각하는 여자를 안다.',
    zho: '劉浩認識那個我覺得這個團體上個星期採訪的女人。',
    cond: 'cond2',
    ...dr30
};
var dr30_cond3 = {
    eng: 'Dave knows the woman that I wonder which organization interviewed',
    kor: '도윤이는 지난주에 어느 단체가 인터뷰했는지 내가 궁금해하는 여자를 안다.',
    zho: '劉浩認識那個我好奇哪個團體上個星期採訪的女人。',
    cond: 'cond3',
    ...dr30
};
var dr30_cond4 = {
    eng: 'I think Dave knows the woman that this organization interviewed her',
    kor: '내가 생각하기에 도윤이는 지난주에 이 단체가 그녀를 인터뷰한 여자를 안다.',
    zho: '我覺得劉浩認識那個這個團體上個星期採訪她的女人。',
    cond: 'cond4',
    ...dr30
};
var dr30_cond5 = {
    eng: 'Dave knows the woman that I think this organization interviewed her',
    kor: '도윤이는 지난주에 이 단체가 그녀를 인터뷰했다고 내가 생각하는 여자를 안다.',
    zho: '劉浩認識那個我覺得這個團體上個星期採訪她的女人。',
    cond: 'cond5',
    ...dr30
};
var dr30_cond6 = {
    eng: 'Dave knows the woman that I wonder which organization interviewed her',
    kor: '도윤이는 지난주에 어느 단체가 그녀를 인터뷰했는지 내가 궁금해하는 여자를 안다.',
    zho: '劉浩認識那個我好奇哪個團體上個星期採訪她的女人。',
    cond: 'cond6',
    ...dr30
};

// --------------------------------------------------------
// SU study: critical items for reading tasks
// --------------------------------------------------------

var sr01 = {
    type: 'critical',
    item: 'item01',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_center: 'borrowed',
    choices1: jsPsych.randomization.shuffle(['Mary', 'I', 'the man', 'the book']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Mary', 'me', 'the man', 'the book']),
    correct2: 'the book'
};
var sr01_cond1 = {
    eng: 'I think Mary knows the man that borrowed this book',
    kor: '내가 생각하기에 민지는 지난달에 이 책을 빌린 남자를 안다.',
    zho: '我覺得黃麗認識那個上個月借這本書的男人。',
    cond: 'cond1',
    ...sr01
};
var sr01_cond2 = {
    eng: 'Mary knows the man that I think borrowed this book',
    kor: '민지는 지난달에 이 책을 빌렸다고 내가 생각하는 남자를 안다.',
    zho: '黃麗認識那個我覺得上個月借這本書的男人。',
    cond: 'cond2',
    ...sr01
};
var sr01_cond3 = {
    eng: 'Mary knows the man that I wonder which book borrowed',
    kor: '민지는 지난달에 어느 책을 빌렸는지 내가 궁금해하는 남자를 안다.',
    zho: '黃麗認識那個我好奇上個月借哪本書的男人',
    cond: 'cond3',
    ...sr01
};
var sr01_cond4 = {
    eng: 'I think Mary knows the man that he borrowed this book',
    kor: '내가 생각하기에 민지는 지난달에 그가 이 책을 빌린 남자를 안다.',
    zho: '我覺得黃麗認識那個他上個月借這本書的男人。',
    cond: 'cond4',
    ...sr01
};
var sr01_cond5 = {
    eng: 'Mary knows the man that I think he borrowed this book',
    kor: '민지는 지난달에 그가 이 책을 빌렸다고 내가 생각하는 남자를 안다.',
    zho: '黃麗認識那個我覺得他上個月借這本書的男人。',
    cond: 'cond5',
    ...sr01
};
var sr01_cond6 = {
    eng: 'Mary knows the man that I wonder which book he borrowed',
    kor: '민지는 지난달에 그가 어느 책을 빌렸는지 내가 궁금해하는 남자를 안다.',
    zho: '黃麗認識那個我好奇他上個月借哪本書的男人。',
    cond: 'cond6',
    ...sr01
};

var sr02 = {
    type: 'critical',
    item: 'item02',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_center: 'borrowed',
    choices1: jsPsych.randomization.shuffle(['John', 'I', 'the woman', 'the bike']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['John', 'me', 'the woman', 'the bike']),
    correct2: 'the bike'
};
var sr02_cond1 = {
    eng: 'I think John knows the woman that borrowed this bike',
    kor: '내가 생각하기에 철수는 지난달에 이 자전거를 빌린 여자를 안다.',
    zho: '我覺得張偉認識那個上個月借這輛自行車的女人。',
    cond: 'cond1',
    ...sr02
};
var sr02_cond2 = {
    eng: 'John knows the woman that I think borrowed this bike',
    kor: '철수는 지난달에 이 자전거를 빌렸다고 내가 생각하는 여자를 안다.',
    zho: '張偉認識那個我覺得上個月借這輛自行車的女人。',
    cond: 'cond2',
    ...sr02
};
var sr02_cond3 = {
    eng: 'John knows the woman that I wonder which bike borrowed',
    kor: '철수는 지난달에 어느 자전거를 빌렸는지 내가 궁금해하는 여자를 안다.',
    zho: '張偉認識那個我好奇上個月借哪輛自行車的女人。',
    cond: 'cond3',
    ...sr02
};
var sr02_cond4 = {
    eng: 'I think John knows the woman that she borrowed this bike',
    kor: '내가 생각하기에 철수는 지난달에 그녀가 이 자전거를 빌린 여자를 안다.',
    zho: '我覺得張偉認識那個她上個月借這輛自行車的女人。',
    cond: 'cond4',
    ...sr02
};
var sr02_cond5 = {
    eng: 'John knows the woman that I think she borrowed this bike',
    kor: '철수는 지난달에 그녀가 이 자전거를 빌렸다고 내가 생각하는 여자를 안다.',
    zho: '張偉認識那個我覺得她上個月借這輛自行車的女人。',
    cond: 'cond5',
    ...sr02
};
var sr02_cond6 = {
    eng: 'John knows the woman that I wonder which bike she borrowed',
    kor: '철수는 지난달에 그녀가 어느 자전거를 빌렸는지 내가 궁금해하는 여자를 안다.',
    zho: '張偉認識那個我好奇她上個月借哪輛自行車的女人。',
    cond: 'cond6',
    ...sr02
};

var sr03 = {
    type: 'critical',
    item: 'item03',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_center: 'bought',
    choices1: jsPsych.randomization.shuffle(['Lisa', 'I', 'the man', 'the farm']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Lisa', 'me', 'the man', 'the farm']),
    correct2: 'the farm'
};
var sr03_cond1 = {
    eng: 'I think Lisa knows the man that bought this farm',
    kor: '내가 생각하기에 은지는 지난달에 이 농장을 산 남자를 안다.',
    zho: '我覺得李娜認識那個上個月買這個農場的男人。',
    cond: 'cond1',
    ...sr03
};
var sr03_cond2 = {
    eng: 'Lisa knows the man that I think bought this farm',
    kor: '은지는 지난달에 이 농장을 샀다고 내가 생각하는 남자를 안다.',
    zho: '李娜認識那個我覺得上個月買這個農場的男人。',
    cond: 'cond2',
    ...sr03
};
var sr03_cond3 = {
    eng: 'Lisa knows the man that I wonder which farm bought',
    kor: '은지는 지난달에 어느 농장을 샀는지 내가 궁금해하는 남자를 안다.',
    zho: '李娜認識那個我好奇上個月買哪個農場的男人。',
    cond: 'cond3',
    ...sr03
};
var sr03_cond4 = {
    eng: 'I think Lisa knows the man that he bought this farm',
    kor: '내가 생각하기에 은지는 지난달에 그가 이 농장을 산 남자를 안다.',
    zho: '我覺得李娜認識那個他上個月買這個農場的男人。',
    cond: 'cond4',
    ...sr03
};
var sr03_cond5 = {
    eng: 'Lisa knows the man that I think he bought this farm',
    kor: '은지는 지난달에 그가 이 농장을 샀다고 내가 생각하는 남자를 안다.',
    zho: '李娜認識那個我覺得他上個月買這個農場的男人。',
    cond: 'cond5',
    ...sr03
};
var sr03_cond6 = {
    eng: 'Lisa knows the man that I wonder which farm he bought',
    kor: '은지는 지난달에 그가 어느 농장을 샀는지 내가 궁금해하는 남자를 안다.',
    zho: '李娜認識那個我好奇他上個月買哪個農場的男人。',
    cond: 'cond6',
    ...sr03
};

var sr04 = {
    type: 'critical',
    item: 'item04',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_center: 'bought',
    choices1: jsPsych.randomization.shuffle(['Bill', 'I', 'the woman', 'the skirt']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['Bill', 'me', 'the woman', 'the skirt']),
    correct2: 'the skirt'
};
var sr04_cond1 = {
    eng: 'I think Bill knows the woman that bought this skirt',
    kor: '내가 생각하기에 우진이는 지난달에 이 치마를 산 여자를 안다.',
    zho: '我覺得吳強認識那個上個月買這條裙子的女人。',
    cond: 'cond1',
    ...sr04
};
var sr04_cond2 = {
    eng: 'Bill knows the woman that I think bought this skirt',
    kor: '우진이는 지난달에 이 치마를 샀다고 내가 생각하는 여자를 안다.',
    zho: '吳強認識那個我覺得上個月買這條裙子的女人。',
    cond: 'cond2',
    ...sr04
};
var sr04_cond3 = {
    eng: 'Bill knows the woman that I wonder which skirt bought',
    kor: '우진이는 지난달에 어느 치마를 샀는지 내가 궁금해하는 여자를 안다.',
    zho: '吳強認識那個我好奇上個月買哪條裙子的女人。',
    cond: 'cond3',
    ...sr04
};
var sr04_cond4 = {
    eng: 'I think Bill knows the woman that she bought this skirt',
    kor: '내가 생각하기에 우진이는 지난달에 그녀가 이 치마를 산 여자를 안다.',
    zho: '我覺得吳強認識那個她上個月買這條裙子的女人。',
    cond: 'cond4',
    ...sr04
};
var sr04_cond5 = {
    eng: 'Bill knows the woman that I think she bought this skirt',
    kor: '우진이는 지난달에 그녀가 이 치마를 샀다고 내가 생각하는 여자를 안다.',
    zho: '吳強認識那個我覺得她上個月買這條裙子的女人。',
    cond: 'cond5',
    ...sr04
};
var sr04_cond6 = {
    eng: 'Bill knows the woman that I wonder which skirt she bought',
    kor: '우진이는 지난달에 그녀가 어느 치마를 샀는지 내가 궁금해하는 여자를 안다.',
    zho: '吳強認識那個我好奇她上個月買哪條裙子的女人。',
    cond: 'cond6',
    ...sr04
};

var sr05 = {
    type: 'critical',
    item: 'item05',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_center: 'built',
    choices1: jsPsych.randomization.shuffle(['Tina', 'I', 'the man', 'the house']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Tina', 'me', 'the man', 'the house']),
    correct2: 'the house'
};
var sr05_cond1 = {
    eng: 'I think Tina knows the man that built this house',
    kor: '내가 생각하기에 수아는 작년에 이 집을 지은 남자를 안다.',
    zho: '我覺得趙悅認識那個去年蓋好這棟房子的男人。',
    cond: 'cond1',
    ...sr05
};
var sr05_cond2 = {
    eng: 'Tina knows the man that I think built this house',
    kor: '수아는 작년에 이 집을 지었다고 내가 생각하는 남자를 안다.',
    zho: '趙悅認識那個我覺得去年蓋好這棟房子的男人。',
    cond: 'cond2',
    ...sr05
};
var sr05_cond3 = {
    eng: 'Tina knows the man that I wonder which house built',
    kor: '수아는 작년에 어느 집을 지었는지 내가 궁금해하는 남자를 안다.',
    zho: '趙悅認識那個我好奇去年蓋好哪所房子的男人。',
    cond: 'cond3',
    ...sr05
};
var sr05_cond4 = {
    eng: 'I think Tina knows the man that he built this house',
    kor: '내가 생각하기에 수아는 작년에 그가 이 집을 지은 남자를 안다.',
    zho: '我覺得趙悅認識那個他去年蓋好這棟房子的男人。',
    cond: 'cond4',
    ...sr05
};
var sr05_cond5 = {
    eng: 'Tina knows the man that I think he built this house',
    kor: '수아는 작년에 그가 이 집을 지었다고 내가 생각하는 남자를 안다.',
    zho: '趙悅認識那個我覺得他去年蓋好這棟房子的男人。',
    cond: 'cond5',
    ...sr05
};
var sr05_cond6 = {
    eng: 'Tina knows the man that I wonder which house he built',
    kor: '수아는 작년에 그가 어느 집을 지었는지 내가 궁금해하는 남자를 안다.',
    zho: '趙悅認識那個我好奇他去年蓋好哪棟房子的男人。',
    cond: 'cond6',
    ...sr05
};

var sr06 = {
    type: 'critical',
    item: 'item06',
    coda: ' at the end of the year.',
    time: ' last year.',
    text_center: 'built',
    choices1: jsPsych.randomization.shuffle(['Mike', 'I', 'the woman', 'the fence']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['Mike', 'me', 'the woman', 'the fence']),
    correct2: 'the fence'
};
var sr06_cond1 = {
    eng: 'I think Mike knows the woman that built this fence',
    kor: '내가 생각하기에 서준이는 작년에 이 울타리를 지은 여자를 안다.',
    zho: '我覺得楊毅認識那個去年蓋好這個圍欄的女人。',
    cond: 'cond1',
    ...sr06
};
var sr06_cond2 = {
    eng: 'Mike knows the woman that I think built this fence',
    kor: '서준이는 작년에 이 울타리를 지었다고 내가 생각하는 여자를 안다.',
    zho: '楊毅認識那個我覺得去年蓋好這個圍欄的女人。',
    cond: 'cond2',
    ...sr06
};
var sr06_cond3 = {
    eng: 'Mike knows the woman that I wonder which fence built',
    kor: '서준이는 작년에 어느 울타리를 지었는지 내가 궁금해하는 여자를 안다.',
    zho: '楊毅認識那個我好奇去年蓋好哪個圍欄的女人。',
    cond: 'cond3',
    ...sr06
};
var sr06_cond4 = {
    eng: 'I think Mike knows the woman that she built this fence',
    kor: '내가 생각하기에 서준이는 작년에 그녀가 이 울타리를 지은 여자를 안다.',
    zho: '我覺得楊毅認識那個她去年蓋好這個圍欄的女人。',
    cond: 'cond4',
    ...sr06
};
var sr06_cond5 = {
    eng: 'Mike knows the woman that I think she built this fence',
    kor: '서준이는 작년에 그녀가 이 울타리를 지었다고 내가 생각하는 여자를 안다.',
    zho: '楊毅認識那個我覺得她去年蓋好這個圍欄的女人。',
    cond: 'cond5',
    ...sr06
};
var sr06_cond6 = {
    eng: 'Mike knows the woman that I wonder which fence she built',
    kor: '서준이는 작년에 그녀가 어느 울타리를 지었는지 내가 궁금해하는 여자를 안다.',
    zho: '楊毅認識那個我好奇她去年蓋好哪個圍欄的女人。',
    cond: 'cond6',
    ...sr06
};

var sr07 = {
    type: 'critical',
    item: 'item07',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_center: 'completed',
    choices1: jsPsych.randomization.shuffle(['Judy', 'I', 'the man', 'the mission']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Judy', 'me', 'the man', 'the mission']),
    correct2: 'the mission'
};
var sr07_cond1 = {
    eng: 'I think Judy knows the man that completed this mission',
    kor: '내가 생각하기에 지은이는 지난달에 이 임무를 완성한 남자를 안다.',
    zho: '我覺得周靜認識那個上個月完成這個任務的男人。',
    cond: 'cond1',
    ...sr07
};
var sr07_cond2 = {
    eng: 'Judy knows the man that I think completed this mission',
    kor: '지은이는 지난달에 이 임무를 완성했다고 내가 생각하는 남자를 안다.',
    zho: '周靜認識那個我覺得上個月完成這個任務的男人。',
    cond: 'cond2',
    ...sr07
};
var sr07_cond3 = {
    eng: 'Judy knows the man that I wonder which mission completed',
    kor: '지은이는 지난달에 어느 임무를 완성했는지 내가 궁금해하는 남자를 안다.',
    zho: '周靜認識那個我好奇上個月完成哪個任務的男人。',
    cond: 'cond3',
    ...sr07
};
var sr07_cond4 = {
    eng: 'I think Judy knows the man that he completed this mission',
    kor: '내가 생각하기에 지은이는 지난달에 그가 이 임무를 완성한 남자를 안다.',
    zho: '我覺得周靜認識那個他上個月完成這個任務的男人。',
    cond: 'cond4',
    ...sr07
};
var sr07_cond5 = {
    eng: 'Judy knows the man that I think he completed this mission',
    kor: '지은이는 지난달에 그가 이 임무를 완성했다고 내가 생각하는 남자를 안다.',
    zho: '周靜認識那個我覺得他上個月完成這個任務的男人。',
    cond: 'cond5',
    ...sr07
};
var sr07_cond6 = {
    eng: 'Judy knows the man that I wonder which mission he completed',
    kor: '지은이는 지난달에 그가 어느 임무를 완성했는지 내가 궁금해하는 남자를 안다.',
    zho: '周靜認識那個我好奇他上個月完成哪個任務的男人。',
    cond: 'cond6',
    ...sr07
};

var sr08 = {
    type: 'critical',
    item: 'item08',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_center: 'completed',
    choices1: jsPsych.randomization.shuffle(['Luke', 'I', 'the woman', 'the project']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['Luke', 'me', 'the woman', 'the project']),
    correct2: 'the project'
};
var sr08_cond1 = {
    eng: 'I think Luke knows the woman that completed this project',
    kor: '내가 생각하기에 현우는 지난달에 이 프로젝트를 완성한 여자를 안다.',
    zho: '我覺得陳傑認識那個上個月完成這個案子的女人。',
    cond: 'cond1',
    ...sr08
};
var sr08_cond2 = {
    eng: 'Luke knows the woman that I think completed this project',
    kor: '현우는 지난달에 이 프로젝트를 완성했다고 내가 생각하는 여자를 안다.',
    zho: '陳傑認識那個我覺得上個月完成這個案子的女人。',
    cond: 'cond2',
    ...sr08
};
var sr08_cond3 = {
    eng: 'Luke knows the woman that I wonder which project completed',
    kor: '현우는 지난달에 어느 프로젝트를 완성했는지 내가 궁금해하는 여자를 안다.',
    zho: '陳傑認識那個我好奇上個月完成哪個案子的女人。',
    cond: 'cond3',
    ...sr08
};
var sr08_cond4 = {
    eng: 'I think Luke knows the woman that she completed this project',
    kor: '내가 생각하기에 현우는 지난달에 그녀가 이 프로젝트를 완성한 여자를 안다.',
    zho: '我覺得陳傑認識那個她上個月完成這個案子的女人。',
    cond: 'cond4',
    ...sr08
};
var sr08_cond5 = {
    eng: 'Luke knows the woman that I think she completed this project',
    kor: '현우는 지난달에 그녀가 이 프로젝트를 완성했다고 내가 생각하는 여자를 안다.',
    zho: '陳傑認識那個我覺得她上個月完成這個案子的女人。',
    cond: 'cond5',
    ...sr08
};
var sr08_cond6 = {
    eng: 'Luke knows the woman that I wonder which project she completed',
    kor: '현우는 지난달에 그녀가 어느 프로젝트를 완성했는지 내가 궁금해하는 여자를 안다.',
    zho: '陳傑認識那個我好奇她上個月完成哪個案子的女人。',
    cond: 'cond6',
    ...sr08
};

var sr09 = {
    type: 'critical',
    item: 'item09',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_center: 'delivered',
    choices1: jsPsych.randomization.shuffle(['Anna', 'I', 'the man', 'the package']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Anna', 'me', 'the man', 'the package']),
    correct2: 'the package'
};
var sr09_cond1 = {
    eng: 'I think Anna knows the man that delivered this package',
    kor: '내가 생각하기에 하은이는 지난주에 이 소포를 배달한 남자를 안다.',
    zho: '我覺得王芳認識那個上個星期送達這個包裹的男人。',
    cond: 'cond1',
    ...sr09
};
var sr09_cond2 = {
    eng: 'Anna knows the man that I think delivered this package',
    kor: '하은이는 지난주에 이 소포를 배달했다고 내가 생각하는 남자를 안다.',
    zho: '王芳認識那個我覺得上個星期送達這個包裹的男人。',
    cond: 'cond2',
    ...sr09
};
var sr09_cond3 = {
    eng: 'Anna knows the man that I wonder which package delivered',
    kor: '하은이는 지난주에 어느 소포를 배달했는지 내가 궁금해하는 남자를 안다.',
    zho: '王芳認識那個我好奇上個星期送達哪個包裹的男人。',
    cond: 'cond3',
    ...sr09
};
var sr09_cond4 = {
    eng: 'I think Anna knows the man that he delivered this package',
    kor: '내가 생각하기에 하은이는 지난주에 그가 이 소포를 배달한 남자를 안다.',
    zho: '我覺得王芳認識那個他上個星期送達這個包裹的男人。',
    cond: 'cond4',
    ...sr09
};
var sr09_cond5 = {
    eng: 'Anna knows the man that I think he delivered this package',
    kor: '하은이는 지난주에 그가 이 소포를 배달했다고 내가 생각하는 남자를 안다.',
    zho: '王芳認識那個我覺得他上個星期送達這個包裹的男人。',
    cond: 'cond5',
    ...sr09
};
var sr09_cond6 = {
    eng: 'Anna knows the man that I wonder which package he delivered',
    kor: '하은이는 지난주에 그가 어느 소포를 배달했는지 내가 궁금해하는 남자를 안다.',
    zho: '王芳認識那個我好奇他上個星期送達哪個包裹的男人。',
    cond: 'cond6',
    ...sr09
};

var sr10 = {
    type: 'critical',
    item: 'item10',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_center: 'delivered',
    choices1: jsPsych.randomization.shuffle(['Dave', 'I', 'the woman', 'the letter']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['Dave', 'me', 'the woman', 'the letter']),
    correct2: 'the letter'
};
var sr10_cond1 = {
    eng: 'I think Dave knows the woman that delivered this letter',
    kor: '내가 생각하기에 도윤이는 지난주에 이 편지를 배달한 여자를 안다.',
    zho: '我覺得劉浩認識那個上個星期送達這封信的女人。',
    cond: 'cond1',
    ...sr10
};
var sr10_cond2 = {
    eng: 'Dave knows the woman that I think delivered this letter',
    kor: '도윤이는 지난주에 이 편지를 배달했다고 내가 생각하는 여자를 안다.',
    zho: '劉浩認識那個我覺得上個星期送達這封信的女人。',
    cond: 'cond2',
    ...sr10
};
var sr10_cond3 = {
    eng: 'Dave knows the woman that I wonder which letter delivered',
    kor: '도윤이는 지난주에 어느 편지를 배달했는지 내가 궁금해하는 여자를 안다.',
    zho: '劉浩認識那個我好奇上個星期送達哪封信的女人。',
    cond: 'cond3',
    ...sr10
};
var sr10_cond4 = {
    eng: 'I think Dave knows the woman that she delivered this letter',
    kor: '내가 생각하기에 도윤이는 지난주에 그녀가 이 편지를 배달한 여자를 안다.',
    zho: '我覺得劉浩認識那個她上個星期送達這封信的女人。',
    cond: 'cond4',
    ...sr10
};
var sr10_cond5 = {
    eng: 'Dave knows the woman that I think she delivered this letter',
    kor: '도윤이는 지난주에 그녀가 이 편지를 배달했다고 내가 생각하는 여자를 안다.',
    zho: '劉浩認識那個我覺得她上個星期送達這封信的女人。',
    cond: 'cond5',
    ...sr10
};
var sr10_cond6 = {
    eng: 'Dave knows the woman that I wonder which letter she delivered',
    kor: '도윤이는 지난주에 그녀가 어느 편지를 배달했는지 내가 궁금해하는 여자를 안다.',
    zho: '劉浩認識那個我好奇她上個星期送達哪封信的女人。',
    cond: 'cond6',
    ...sr10
};

var sr11 = {
    type: 'critical',
    item: 'item11',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_center: 'fixed',
    choices1: jsPsych.randomization.shuffle(['Mary', 'I', 'the man', 'the machine']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Mary', 'me', 'the man', 'the machine']),
    correct2: 'the machine'
};
var sr11_cond1 = {
    eng: 'I think Mary knows the man that fixed this machine',
    kor: '내가 생각하기에 민지는 지난달에 이 기계를 고친 남자를 안다.',
    zho: '我覺得黃麗認識那個上個月修好這台機器的男人。',
    cond: 'cond1',
    ...sr11
};
var sr11_cond2 = {
    eng: 'Mary knows the man that I think fixed this machine',
    kor: '민지는 지난달에 이 기계를 고쳤다고 내가 생각하는 남자를 안다.',
    zho: '黃麗認識那個我覺得上個月修好這台機器的男人。',
    cond: 'cond2',
    ...sr11
};
var sr11_cond3 = {
    eng: 'Mary knows the man that I wonder which machine fixed',
    kor: '민지는 지난달에 어느 기계를 고쳤는지 내가 궁금해하는 남자를 안다.',
    zho: '黃麗認識那個我好奇上個月修好哪台機器的男人。',
    cond: 'cond3',
    ...sr11
};
var sr11_cond4 = {
    eng: 'I think Mary knows the man that he fixed this machine',
    kor: '내가 생각하기에 민지는 지난달에 그가 이 기계를 고친 남자를 안다.',
    zho: '我覺得黃麗認識那個他上個月修好這台機器的男人。',
    cond: 'cond4',
    ...sr11
};
var sr11_cond5 = {
    eng: 'Mary knows the man that I think he fixed this machine',
    kor: '민지는 지난달에 그가 이 기계를 고쳤다고 내가 생각하는 남자를 안다.',
    zho: '黃麗認識那個我覺得他上個月修好這台機器的男人。',
    cond: 'cond5',
    ...sr11
};
var sr11_cond6 = {
    eng: 'Mary knows the man that I wonder which machine he fixed',
    kor: '민지는 지난달에 그가 어느 기계를 고쳤는지 내가 궁금해하는 남자를 안다.',
    zho: '黃麗認識那個我好奇他上個月修好哪台機器的男人。',
    cond: 'cond6',
    ...sr11
};

var sr12 = {
    type: 'critical',
    item: 'item12',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_center: 'fixed',
    choices1: jsPsych.randomization.shuffle(['John', 'I', 'the woman', 'the computer']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['John', 'me', 'the woman', 'the computer']),
    correct2: 'the computer'
};
var sr12_cond1 = {
    eng: 'I think John knows the woman that fixed this computer',
    kor: '내가 생각하기에 철수는 지난달에 이 컴퓨터를 고친 여자를 안다.',
    zho: '我覺得張偉認識那個上個月修好這台電腦的女人。',
    cond: 'cond1',
    ...sr12
};
var sr12_cond2 = {
    eng: 'John knows the woman that I think fixed this computer',
    kor: '철수는 지난달에 이 컴퓨터를 고쳤다고 내가 생각하는 여자를 안다.',
    zho: '張偉認識那個我覺得上個月修好這台電腦的女人。',
    cond: 'cond2',
    ...sr12
};
var sr12_cond3 = {
    eng: 'John knows the woman that I wonder which computer fixed',
    kor: '철수는 지난달에 어느 컴퓨터를 고쳤는지 내가 궁금해하는 여자를 안다.',
    zho: '張偉認識那個我好奇上個月修好哪台電腦的女人。',
    cond: 'cond3',
    ...sr12
};
var sr12_cond4 = {
    eng: 'I think John knows the woman that she fixed this computer',
    kor: '내가 생각하기에 철수는 지난달에 그녀가 이 컴퓨터를 고친 여자를 안다.',
    zho: '我覺得張偉認識那個她上個月修好這台電腦的女人。',
    cond: 'cond4',
    ...sr12
};
var sr12_cond5 = {
    eng: 'John knows the woman that I think she fixed this computer',
    kor: '철수는 지난달에 그녀가 이 컴퓨터를 고쳤다고 내가 생각하는 여자를 안다.',
    zho: '張偉認識那個我覺得她上個月修好這台電腦的女人。',
    cond: 'cond5',
    ...sr12
};
var sr12_cond6 = {
    eng: 'John knows the woman that I wonder which computer she fixed',
    kor: '철수는 지난달에 그녀가 어느 컴퓨터를 고쳤는지 내가 궁금해하는 여자를 안다.',
    zho: '張偉認識那個我好奇她上個月修好哪台電腦的女人。',
    cond: 'cond6',
    ...sr12
};

var sr13 = {
    type: 'critical',
    item: 'item13',
    coda: ' at the beginning of the night.',
    time: ' last night.',
    text_center: 'found',
    choices1: jsPsych.randomization.shuffle(['Lisa', 'I', 'the man', 'the wallet']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Lisa', 'me', 'the man', 'the wallet']),
    correct2: 'the wallet'
};
var sr13_cond1 = {
    eng: 'I think Lisa knows the man that found this wallet',
    kor: '내가 생각하기에 은지는 어젯밤에 이 지갑을 찾은 남자를 안다.',
    zho: '我覺得李娜認識那個昨晚找到這個錢包的男人。',
    cond: 'cond1',
    ...sr13
};
var sr13_cond2 = {
    eng: 'Lisa knows the man that I think found this wallet',
    kor: '은지는 어젯밤에 이 지갑을 찾았다고 내가 생각하는 남자를 안다.',
    zho: '李娜認識那個我覺得昨晚找到這個錢包的男人。',
    cond: 'cond2',
    ...sr13
};
var sr13_cond3 = {
    eng: 'Lisa knows the man that I wonder which wallet found',
    kor: '은지는 어젯밤에 어느 지갑을 찾았는지 내가 궁금해하는 남자를 안다.',
    zho: '李娜認識那個我好奇昨晚找到哪個錢包的男人。',
    cond: 'cond3',
    ...sr13
};
var sr13_cond4 = {
    eng: 'I think Lisa knows the man that he found this wallet',
    kor: '내가 생각하기에 은지는 어젯밤에 그가 이 지갑을 찾은 남자를 안다.',
    zho: '我覺得李娜認識那個他昨晚找到這個錢包的男人。',
    cond: 'cond4',
    ...sr13
};
var sr13_cond5 = {
    eng: 'Lisa knows the man that I think he found this wallet',
    kor: '은지는 어젯밤에 그가 이 지갑을 찾았다고 내가 생각하는 남자를 안다.',
    zho: '李娜認識那個我覺得他昨晚找到這個錢包的男人。',
    cond: 'cond5',
    ...sr13
};
var sr13_cond6 = {
    eng: 'Lisa knows the man that I wonder which wallet he found',
    kor: '은지는 어젯밤에 그가 어느 지갑을 찾았는지 내가 궁금해하는 남자를 안다.',
    zho: '李娜認識那個我好奇他昨晚找到哪個錢包的男人。',
    cond: 'cond6',
    ...sr13
};

var sr14 = {
    type: 'critical',
    item: 'item14',
    coda: ' at the end of the night.',
    time: ' last night.',
    text_center: 'found',
    choices1: jsPsych.randomization.shuffle(['Bill', 'I', 'the woman', 'the necklace']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['Bill', 'me', 'the woman', 'the necklace']),
    correct2: 'the necklace'
};
var sr14_cond1 = {
    eng: 'I think Bill knows the woman that found this necklace',
    kor: '내가 생각하기에 우진이는 어젯밤에 이 목걸이를 찾은 여자를 안다.',
    zho: '我覺得吳強認識那個昨晚找到這條項鍊的女人。',
    cond: 'cond1',
    ...sr14
};
var sr14_cond2 = {
    eng: 'Bill knows the woman that I think found this necklace',
    kor: '우진이는 어젯밤에 이 목걸이를 찾았다고 내가 생각하는 여자를 안다.',
    zho: '吳強認識那個我覺得昨晚找到這條項鍊的女人。',
    cond: 'cond2',
    ...sr14
};
var sr14_cond3 = {
    eng: 'Bill knows the woman that I wonder which necklace found',
    kor: '우진이는 어젯밤에 어느 목걸이를 찾았는지 내가 궁금해하는 여자를 안다.',
    zho: '吳強認識那個我好奇昨晚找到哪條項鍊的女人。',
    cond: 'cond3',
    ...sr14
};
var sr14_cond4 = {
    eng: 'I think Bill knows the woman that she found this necklace',
    kor: '내가 생각하기에 우진이는 어젯밤에 그녀가 이 목걸이를 찾은 여자를 안다.',
    zho: '我覺得吳強認識那個她昨晚找到這條項鍊的女人。',
    cond: 'cond4',
    ...sr14
};
var sr14_cond5 = {
    eng: 'Bill knows the woman that I think she found this necklace',
    kor: '우진이는 어젯밤에 그녀가 이 목걸이를 찾았다고 내가 생각하는 여자를 안다.',
    zho: '吳強認識那個我覺得她昨晚找到這條項鍊的女人。',
    cond: 'cond5',
    ...sr14
};
var sr14_cond6 = {
    eng: 'Bill knows the woman that I wonder which necklace she found',
    kor: '우진이는 어젯밤에 그녀가 어느 목걸이를 찾았는지 내가 궁금해하는 여자를 안다.',
    zho: '吳強認識那個我好奇她昨晚找到哪條項鍊的女人。',
    cond: 'cond6',
    ...sr14
};

var sr15 = {
    type: 'critical',
    item: 'item15',
    coda: ' at the end of the year.',
    time: ' last year.',
    text_center: 'made',
    choices1: jsPsych.randomization.shuffle(['Tina', 'I', 'the man', 'the video']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Tina', 'me', 'the man', 'the video']),
    correct2: 'the video'
};
var sr15_cond1 = {
    eng: 'I think Tina knows the man that made this video',
    kor: '내가 생각하기에 수아는 작년에 이 동영상을 만든 남자를 안다.',
    zho: '我覺得趙悅認識那個去年做這個影片的男人。',
    cond: 'cond1',
    ...sr15
};
var sr15_cond2 = {
    eng: 'Tina knows the man that I think made this video',
    kor: '수아는 작년에 이 동영상을 만들었다고 내가 생각하는 남자를 안다.',
    zho: '趙悅認識那個我覺得去年做這個影片的男人。',
    cond: 'cond2',
    ...sr15
};
var sr15_cond3 = {
    eng: 'Tina knows the man that I wonder which video made',
    kor: '수아는 작년에 어느 동영상을 만들었는지 내가 궁금해하는 남자를 안다.',
    zho: '趙悅認識那個我好奇去年做哪個影片的男人。',
    cond: 'cond3',
    ...sr15
};
var sr15_cond4 = {
    eng: 'I think Tina knows the man that he made this video',
    kor: '내가 생각하기에 수아는 작년에 그가 이 동영상을 만든 남자를 안다.',
    zho: '我覺得趙悅認識那個他去年做這個影片的男人。',
    cond: 'cond4',
    ...sr15
};
var sr15_cond5 = {
    eng: 'Tina knows the man that I think he made this video',
    kor: '수아는 작년에 그가 이 동영상을 만들었다고 내가 생각하는 남자를 안다.',
    zho: '趙悅認識那個我覺得他去年做這個影片的男人。',
    cond: 'cond5',
    ...sr15
};
var sr15_cond6 = {
    eng: 'Tina knows the man that I wonder which video he made',
    kor: '수아는 작년에 그가 어느 동영상을 만들었는지 내가 궁금해하는 남자를 안다.',
    zho: '趙悅認識那個我好奇他去年做哪個影片的男人。',
    cond: 'cond6',
    ...sr15
};

var sr16 = {
    type: 'critical',
    item: 'item16',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_center: 'made',
    choices1: jsPsych.randomization.shuffle(['Mike', 'I', 'the woman', 'the website']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['Mike', 'me', 'the woman', 'the website']),
    correct2: 'the website'
};
var sr16_cond1 = {
    eng: 'I think Mike knows the woman that made this website',
    kor: '내가 생각하기에 서준이는 작년에 이 웹사이트를 만든 여자를 안다.',
    zho: '我覺得楊毅認識那個去年做這個網站的女人。',
    cond: 'cond1',
    ...sr16
};
var sr16_cond2 = {
    eng: 'Mike knows the woman that I think made this website',
    kor: '서준이는 작년에 이 웹사이트를 만들었다고 내가 생각하는 여자를 안다.',
    zho: '楊毅認識那個我覺得去年做這個網站的女人。',
    cond: 'cond2',
    ...sr16
};
var sr16_cond3 = {
    eng: 'Mike knows the woman that I wonder which website made',
    kor: '서준이는 작년에 어느 웹사이트를 만들었는지 내가 궁금해하는 여자를 안다.',
    zho: '楊毅認識那個我好奇去年做哪個網站的女人。',
    cond: 'cond3',
    ...sr16
};
var sr16_cond4 = {
    eng: 'I think Mike knows the woman that she made this website',
    kor: '내가 생각하기에 서준이는 작년에 그녀가 이 웹사이트를 만든 여자를 안다.',
    zho: '我覺得楊毅認識那個她去年做這個網站的女人。',
    cond: 'cond4',
    ...sr16
};
var sr16_cond5 = {
    eng: 'Mike knows the woman that I think she made this website',
    kor: '서준이는 작년에 그녀가 이 웹사이트를 만들었다고 내가 생각하는 여자를 안다.',
    zho: '楊毅認識那個我覺得她去年做這個網站的女人。',
    cond: 'cond5',
    ...sr16
};
var sr16_cond6 = {
    eng: 'Mike knows the woman that I wonder which website she made',
    kor: '서준이는 작년에 그녀가 어느 웹사이트를 만들었는지 내가 궁금해하는 여자를 안다.',
    zho: '楊毅認識那個我好奇她去年做哪個網站的女人。',
    cond: 'cond6',
    ...sr16
};

var sr17 = {
    type: 'critical',
    item: 'item17',
    coda: ' at the beginning of the night.',
    time: ' last night.',
    text_center: 'ordered',
    choices1: jsPsych.randomization.shuffle(['Judy', 'I', 'the man', 'the pizza']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Judy', 'me', 'the man', 'the pizza']),
    correct2: 'the pizza'
};
var sr17_cond1 = {
    eng: 'I think Judy knows the man that ordered this pizza',
    kor: '내가 생각하기에 지은이는 어젯밤에 이 피자를 주문한 남자를 안다.',
    zho: '我覺得周靜認識那個昨晚點這個披薩的男人。',
    cond: 'cond1',
    ...sr17
};
var sr17_cond2 = {
    eng: 'Judy knows the man that I think ordered this pizza',
    kor: '지은이는 어젯밤에 이 피자를 주문했다고 내가 생각하는 남자를 안다.',
    zho: '周靜認識那個我覺得昨晚點這個披薩的男人。',
    cond: 'cond2',
    ...sr17
};
var sr17_cond3 = {
    eng: 'Judy knows the man that I wonder which pizza ordered',
    kor: '지은이는 어젯밤에 어느 피자를 주문했는지 내가 궁금해하는 남자를 안다.',
    zho: '周靜認識那個我好奇昨晚點哪個披薩的男人。',
    cond: 'cond3',
    ...sr17
};
var sr17_cond4 = {
    eng: 'I think Judy knows the man that he ordered this pizza',
    kor: '내가 생각하기에 지은이는 어젯밤에 그가 이 피자를 주문한 남자를 안다.',
    zho: '我覺得周靜認識那個他昨晚點這個披薩的男人。',
    cond: 'cond4',
    ...sr17
};
var sr17_cond5 = {
    eng: 'Judy knows the man that I think he ordered this pizza',
    kor: '지은이는 어젯밤에 그가 이 피자를 주문했다고 내가 생각하는 남자를 안다.',
    zho: '周靜認識那個我覺得他昨晚點這個披薩的男人。',
    cond: 'cond5',
    ...sr17
};
var sr17_cond6 = {
    eng: 'Judy knows the man that I wonder which pizza he ordered',
    kor: '지은이는 어젯밤에 그가 어느 피자를 주문했는지 내가 궁금해하는 남자를 안다.',
    zho: '周靜認識那個我好奇他昨晚點哪個披薩的男人。',
    cond: 'cond6',
    ...sr17
};

var sr18 = {
    type: 'critical',
    item: 'item18',
    coda: ' at the end of the night.',
    time: ' last night.',
    text_center: 'ordered',
    choices1: jsPsych.randomization.shuffle(['Luke', 'I', 'the woman', 'the dish']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['Luke', 'me', 'the woman', 'the dish']),
    correct2: 'the dish'
};
var sr18_cond1 = {
    eng: 'I think Luke knows the woman that ordered this dish',
    kor: '내가 생각하기에 현우는 어젯밤에 이 요리를 주문한 여자를 안다.',
    zho: '我覺得陳傑認識那個昨晚點這道菜的女人。',
    cond: 'cond1',
    ...sr18
};
var sr18_cond2 = {
    eng: 'Luke knows the woman that I think ordered this dish',
    kor: '현우는 어젯밤에 이 요리를 주문했다고 내가 생각하는 여자를 안다.',
    zho: '陳傑認識那個我覺得昨晚點這道菜的女人。',
    cond: 'cond2',
    ...sr18
};
var sr18_cond3 = {
    eng: 'Luke knows the woman that I wonder which dish ordered',
    kor: '현우는 어젯밤에 어느 요리를 주문했는지 내가 궁금해하는 여자를 안다.',
    zho: '陳傑認識那個我好奇昨晚點哪道菜的女人。',
    cond: 'cond3',
    ...sr18
};
var sr18_cond4 = {
    eng: 'I think Luke knows the woman that she ordered this dish',
    kor: '내가 생각하기에 현우는 어젯밤에 그녀가 이 요리를 주문한 여자를 안다.',
    zho: '我覺得陳傑認識那個她昨晚點這道菜的女人。',
    cond: 'cond4',
    ...sr18
};
var sr18_cond5 = {
    eng: 'Luke knows the woman that I think she ordered this dish',
    kor: '현우는 어젯밤에 그녀가 이 요리를 주문했다고 내가 생각하는 여자를 안다.',
    zho: '陳傑認識那個我覺得她昨晚點這道菜的女人。',
    cond: 'cond5',
    ...sr18
};
var sr18_cond6 = {
    eng: 'Luke knows the woman that I wonder which dish she ordered',
    kor: '현우는 어젯밤에 그녀가 어느 요리를 주문했는지 내가 궁금해하는 여자를 안다.',
    zho: '陳傑認識那個我好奇她昨晚點哪道菜的女人。',
    cond: 'cond6',
    ...sr18
};

var sr19 = {
    type: 'critical',
    item: 'item19',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_center: 'received',
    choices1: jsPsych.randomization.shuffle(['Anna', 'I', 'the man', 'the message']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Anna', 'me', 'the man', 'the message']),
    correct2: 'the message'
};
var sr19_cond1 = {
    eng: 'I think Anna knows the man that received this message',
    kor: '내가 생각하기에 하은이는 지난주에 이 메시지를 받은 남자를 안다.',
    zho: '我覺得王芳認識那個上個星期得到這條消息的男人。',
    cond: 'cond1',
    ...sr19
};
var sr19_cond2 = {
    eng: 'Anna knows the man that I think received this message',
    kor: '하은이는 지난주에 이 메시지를 받았다고 내가 생각하는 남자를 안다.',
    zho: '王芳認識那個我覺得上個星期得到這條消息的男人。',
    cond: 'cond2',
    ...sr19
};
var sr19_cond3 = {
    eng: 'Anna knows the man that I wonder which message received',
    kor: '하은이는 지난주에 어느 메시지를 받았는지 내가 궁금해하는 남자를 안다.',
    zho: '王芳認識那個我好奇上個星期得到哪條消息的男人。',
    cond: 'cond3',
    ...sr19
};
var sr19_cond4 = {
    eng: 'I think Anna knows the man that he received this message',
    kor: '내가 생각하기에 하은이는 지난주에 그가 이 메시지를 받은 남자를 안다.',
    zho: '我覺得王芳認識那個他上個星期得到這條消息的男人。',
    cond: 'cond4',
    ...sr19
};
var sr19_cond5 = {
    eng: 'Anna knows the man that I think he received this message',
    kor: '하은이는 지난주에 그가 이 메시지를 받았다고 내가 생각하는 남자를 안다.',
    zho: '王芳認識那個我覺得他上個星期得到這條消息的男人。',
    cond: 'cond5',
    ...sr19
};
var sr19_cond6 = {
    eng: 'Anna knows the man that I wonder which message he received',
    kor: '하은이는 지난주에 그가 어느 메시지를 받았는지 내가 궁금해하는 남자를 안다.',
    zho: '王芳認識那個我好奇他上個星期得到哪條消息的男人。',
    cond: 'cond6',
    ...sr19
};

var sr20 = {
    type: 'critical',
    item: 'item20',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_center: 'received',
    choices1: jsPsych.randomization.shuffle(['Dave', 'I', 'the woman', 'the award']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['Dave', 'me', 'the woman', 'the award']),
    correct2: 'the award'
};
var sr20_cond1 = {
    eng: 'I think Dave knows the woman that received this award',
    kor: '내가 생각하기에 도윤이는 지난주에 이 상을 받은 여자를 안다.',
    zho: '我覺得劉浩認識那個上個星期得到這個獎項的女人。',
    cond: 'cond1',
    ...sr20
};
var sr20_cond2 = {
    eng: 'Dave knows the woman that I think received this award',
    kor: '도윤이는 지난주에 이 상을 받았다고 내가 생각하는 여자를 안다.',
    zho: '劉浩認識那個我覺得上個星期得到這個獎項的女人。',
    cond: 'cond2',
    ...sr20
};
var sr20_cond3 = {
    eng: 'Dave knows the woman that I wonder which award received',
    kor: '도윤이는 지난주에 어느 상을 받았는지 내가 궁금해하는 여자를 안다.',
    zho: '劉浩認識那個我好奇上個星期得到哪個獎項的女人。',
    cond: 'cond3',
    ...sr20
};
var sr20_cond4 = {
    eng: 'I think Dave knows the woman that she received this award',
    kor: '내가 생각하기에 도윤이는 지난주에 그녀가 이 상을 받은 여자를 안다.',
    zho: '我覺得劉浩認識那個她上個星期得到這個獎項的女人。',
    cond: 'cond4',
    ...sr20
};
var sr20_cond5 = {
    eng: 'Dave knows the woman that I think she received this award',
    kor: '도윤이는 지난주에 그녀가 이 상을 받았다고 내가 생각하는 여자를 안다.',
    zho: '劉浩認識那個我覺得她上個星期得到這個獎項的女人。',
    cond: 'cond5',
    ...sr20
};
var sr20_cond6 = {
    eng: 'Dave knows the woman that I wonder which award she received',
    kor: '도윤이는 지난주에 그녀가 어느 상을 받았는지 내가 궁금해하는 여자를 안다.',
    zho: '劉浩認識那個我好奇她上個星期得到哪個獎項的女人。',
    cond: 'cond6',
    ...sr20
};

var sr21 = {
    type: 'critical',
    item: 'item21',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_center: 'rented',
    choices1: jsPsych.randomization.shuffle(['Mary', 'I', 'the man', 'the apartment']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Mary', 'me', 'the man', 'the apartment']),
    correct2: 'the apartment'
};
var sr21_cond1 = {
    eng: 'I think Mary knows the man that rented this apartment',
    kor: '내가 생각하기에 민지는 작년에 이 아파트를 빌린 남자를 안다.',
    zho: '我覺得黃麗認識那個去年租這間公寓的男人。',
    cond: 'cond1',
    ...sr21
};
var sr21_cond2 = {
    eng: 'Mary knows the man that I think rented this apartment',
    kor: '민지는 작년에 이 아파트를 빌렸다고 내가 생각하는 남자를 안다.',
    zho: '黃麗認識那個我覺得去年租這間公寓的男人。',
    cond: 'cond2',
    ...sr21
};
var sr21_cond3 = {
    eng: 'Mary knows the man that I wonder which apartment rented',
    kor: '민지는 작년에 어느 아파트를 빌렸는지 내가 궁금해하는 남자를 안다.',
    zho: '黃麗認識那個我好奇去年租哪間公寓的男人。',
    cond: 'cond3',
    ...sr21
};
var sr21_cond4 = {
    eng: 'I think Mary knows the man that he rented this apartment',
    kor: '내가 생각하기에 민지는 작년에 그가 이 아파트를 빌린 남자를 안다.',
    zho: '我覺得黃麗認識那個他去年租這間公寓的男人。',
    cond: 'cond4',
    ...sr21
};
var sr21_cond5 = {
    eng: 'Mary knows the man that I think he rented this apartment',
    kor: '민지는 작년에 그가 이 아파트를 빌렸다고 내가 생각하는 남자를 안다.',
    zho: '黃麗認識那個我覺得他去年租這間公寓的男人。',
    cond: 'cond5',
    ...sr21
};
var sr21_cond6 = {
    eng: 'Mary knows the man that I wonder which apartment he rented',
    kor: '민지는 작년에 그가 어느 아파트를 빌렸는지 내가 궁금해하는 남자를 안다.',
    zho: '黃麗認識那個我好奇他去年租哪間公寓的男人。',
    cond: 'cond6',
    ...sr21
};

var sr22 = {
    type: 'critical',
    item: 'item22',
    coda: ' at the end of the year.',
    time: ' last year.',
    text_center: 'rented',
    choices1: jsPsych.randomization.shuffle(['John', 'I', 'the woman', 'the car']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['John', 'me', 'the woman', 'the car']),
    correct2: 'the car'
};
var sr22_cond1 = {
    eng: 'I think John knows the woman that rented this car',
    kor: '내가 생각하기에 철수는 작년에 이 차를 빌린 여자를 안다.',
    zho: '我覺得張偉認識那個去年租這輛車的女人。',
    cond: 'cond1',
    ...sr22
};
var sr22_cond2 = {
    eng: 'John knows the woman that I think rented this car',
    kor: '철수는 작년에 이 차를 빌렸다고 내가 생각하는 여자를 안다.',
    zho: '張偉認識那個我覺得去年租這輛車的女人。',
    cond: 'cond2',
    ...sr22
};
var sr22_cond3 = {
    eng: 'John knows the woman that I wonder which car rented',
    kor: '철수는 작년에 어느 차를 빌렸는지 내가 궁금해하는 여자를 안다.',
    zho: '張偉認識那個我好奇去年租哪輛車的女人。',
    cond: 'cond3',
    ...sr22
};
var sr22_cond4 = {
    eng: 'I think John knows the woman that she rented this car',
    kor: '내가 생각하기에 철수는 작년에 그녀가 이 차를 빌린 여자를 안다.',
    zho: '我覺得張偉認識那個她去年租這輛車的女人。',
    cond: 'cond4',
    ...sr22
};
var sr22_cond5 = {
    eng: 'John knows the woman that I think she rented this car',
    kor: '철수는 작년에 그녀가 이 차를 빌렸다고 내가 생각하는 여자를 안다.',
    zho: '張偉認識那個我覺得她去年租這輛車的女人。',
    cond: 'cond5',
    ...sr22
};
var sr22_cond6 = {
    eng: 'John knows the woman that I wonder which car she rented',
    kor: '철수는 작년에 그녀가 어느 차를 빌렸는지 내가 궁금해하는 여자를 안다.',
    zho: '張偉認識那個我好奇她去年租哪輛車的女人。',
    cond: 'cond6',
    ...sr22
};

var sr23 = {
    type: 'critical',
    item: 'item23',
    coda: ' at the end of the year.',
    time: ' last year.',
    text_center: 'sold',
    choices1: jsPsych.randomization.shuffle(['Lisa', 'I', 'the man', 'the cafe']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Lisa', 'me', 'the man', 'the cafe']),
    correct2: 'the cafe'
};
var sr23_cond1 = {
    eng: 'I think Lisa knows the man that sold this cafe',
    kor: '내가 생각하기에 은지는 작년에 이 카페를 판 남자를 안다.',
    zho: '我覺得李娜認識那個去年賣掉這家咖啡館的男人。',
    cond: 'cond1',
    ...sr23
};
var sr23_cond2 = {
    eng: 'Lisa knows the man that I think sold this cafe',
    kor: '은지는 작년에 이 카페를 팔았다고 내가 생각하는 남자를 안다.',
    zho: '李娜認識那個我覺得去年賣掉這家咖啡館的男人。',
    cond: 'cond2',
    ...sr23
};
var sr23_cond3 = {
    eng: 'Lisa knows the man that I wonder which cafe sold',
    kor: '은지는 작년에 어느 카페를 팔았는지 내가 궁금해하는 남자를 안다.',
    zho: '李娜認識那個我好奇去年賣掉哪家咖啡館的男人。',
    cond: 'cond3',
    ...sr23
};
var sr23_cond4 = {
    eng: 'I think Lisa knows the man that he sold this cafe',
    kor: '내가 생각하기에 은지는 작년에 그가 이 카페를 판 남자를 안다.',
    zho: '我覺得李娜認識那個他去年賣掉這家咖啡館的男人。',
    cond: 'cond4',
    ...sr23
};
var sr23_cond5 = {
    eng: 'Lisa knows the man that I think he sold this cafe',
    kor: '은지는 작년에 그가 이 카페를 팔았다고 내가 생각하는 남자를 안다.',
    zho: '李娜認識那個我覺得他去年賣掉這家咖啡館的男人。',
    cond: 'cond5',
    ...sr23
};
var sr23_cond6 = {
    eng: 'Lisa knows the man that I wonder which cafe he sold',
    kor: '은지는 작년에 그가 어느 카페를 팔았는지 내가 궁금해하는 남자를 안다.',
    zho: '李娜認識那個我好奇他去年賣掉哪家咖啡館的男人。',
    cond: 'cond6',
    ...sr23
};

var sr24 = {
    type: 'critical',
    item: 'item24',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_center: 'sold',
    choices1: jsPsych.randomization.shuffle(['Bill', 'I', 'the woman', 'the building']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['Bill', 'me', 'the woman', 'the building']),
    correct2: 'the building'
};
var sr24_cond1 = {
    eng: 'I think Bill knows the woman that sold this building',
    kor: '내가 생각하기에 우진이는 작년에 이 건물을 판 여자를 안다.',
    zho: '我覺得吳強認識那個去年賣掉這棟樓的女人。',
    cond: 'cond1',
    ...sr24
};
var sr24_cond2 = {
    eng: 'Bill knows the woman that I think sold this building',
    kor: '우진이는 작년에 이 건물을 팔았다고 내가 생각하는 여자를 안다.',
    zho: '吳強認識那個我覺得去年賣掉這棟樓的女人。',
    cond: 'cond2',
    ...sr24
};
var sr24_cond3 = {
    eng: 'Bill knows the woman that I wonder which building sold',
    kor: '우진이는 작년에 어느 건물을 팔았는지 내가 궁금해하는 여자를 안다.',
    zho: '吳強認識那個我好奇去年賣掉哪棟樓的女人。',
    cond: 'cond3',
    ...sr24
};
var sr24_cond4 = {
    eng: 'I think Bill knows the woman that she sold this building',
    kor: '내가 생각하기에 우진이는 작년에 그녀가 이 건물을 판 여자를 안다.',
    zho: '我覺得吳強認識那個她去年賣掉這棟樓的女人。',
    cond: 'cond4',
    ...sr24
};
var sr24_cond5 = {
    eng: 'Bill knows the woman that I think she sold this building',
    kor: '우진이는 작년에 그녀가 이 건물을 팔았다고 내가 생각하는 여자를 안다.',
    zho: '吳強認識那個我覺得她去年賣掉這棟樓的女人。',
    cond: 'cond5',
    ...sr24
};
var sr24_cond6 = {
    eng: 'Bill knows the woman that I wonder which building she sold',
    kor: '우진이는 작년에 그녀가 어느 건물을 팔았는지 내가 궁금해하는 여자를 안다.',
    zho: '吳強認識那個我好奇她去年賣掉哪棟樓的女人。',
    cond: 'cond6',
    ...sr24
};

var sr25 = {
    type: 'critical',
    item: 'item25',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_center: 'sent',
    choices1: jsPsych.randomization.shuffle(['Tina', 'I', 'the man', 'the email']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Tina', 'me', 'the man', 'the email']),
    correct2: 'the email'
};
var sr25_cond1 = {
    eng: 'I think Tina knows the man that sent this email',
    kor: '내가 생각하기에 수아는 지난주에 이 이메일을 보낸 남자를 안다.',
    zho: '我覺得趙悅認識那個上個星期送出這封電子郵件的男人。',
    cond: 'cond1',
    ...sr25
};
var sr25_cond2 = {
    eng: 'Tina knows the man that I think sent this email',
    kor: '수아는 지난주에 이 이메일을 보냈다고 내가 생각하는 남자를 안다.',
    zho: '趙悅認識那個我覺得上個星期送出這封電子郵件的男人。',
    cond: 'cond2',
    ...sr25
};
var sr25_cond3 = {
    eng: 'Tina knows the man that I wonder which email sent',
    kor: '수아는 지난주에 어느 이메일을 보냈는지 내가 궁금해하는 남자를 안다.',
    zho: '趙悅認識那個我好奇上個星期送出哪封電子郵件的男人。',
    cond: 'cond3',
    ...sr25
};
var sr25_cond4 = {
    eng: 'I think Tina knows the man that he sent this email',
    kor: '내가 생각하기에 수아는 지난주에 그가 이 이메일을 보낸 남자를 안다.',
    zho: '我覺得趙悅認識那個他上個星期送出這封電子郵件的男人。',
    cond: 'cond4',
    ...sr25
};
var sr25_cond5 = {
    eng: 'Tina knows the man that I think he sent this email',
    kor: '수아는 지난주에 그가 이 이메일을 보냈다고 내가 생각하는 남자를 안다.',
    zho: '趙悅認識那個我覺得他上個星期送出這封電子郵件的男人。',
    cond: 'cond5',
    ...sr25
};
var sr25_cond6 = {
    eng: 'Tina knows the man that I wonder which email he sent',
    kor: '수아는 지난주에 그가 어느 이메일을 보냈는지 내가 궁금해하는 남자를 안다.',
    zho: '趙悅認識那個我好奇他上個星期送出哪封電子郵件的男人。',
    cond: 'cond6',
    ...sr25
};

var sr26 = {
    type: 'critical',
    item: 'item26',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_center: 'sent',
    choices1: jsPsych.randomization.shuffle(['Mike', 'I', 'the woman', 'the check']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['Mike', 'me', 'the woman', 'the check']),
    correct2: 'the check'
};
var sr26_cond1 = {
    eng: 'I think Mike knows the woman that sent this check',
    kor: '내가 생각하기에 서준이는 지난주에 이 수표를 보낸 여자를 안다.',
    zho: '我覺得楊毅認識那個上個星期送出這張支票的女人。',
    cond: 'cond1',
    ...sr26
};
var sr26_cond2 = {
    eng: 'Mike knows the woman that I think sent this check',
    kor: '서준이는 지난주에 이 수표를 보냈다고 내가 생각하는 여자를 안다.',
    zho: '楊毅認識那個我覺得上個星期送出這張支票的女人。',
    cond: 'cond2',
    ...sr26
};
var sr26_cond3 = {
    eng: 'Mike knows the woman that I wonder which check sent',
    kor: '서준이는 지난주에 어느 수표를 보냈는지 내가 궁금해하는 여자를 안다.',
    zho: '楊毅認識那個我好奇上個星期送出哪張支票的女人。',
    cond: 'cond3',
    ...sr26
};
var sr26_cond4 = {
    eng: 'I think Mike knows the woman that she sent this check',
    kor: '내가 생각하기에 서준이는 지난주에 그녀가 이 수표를 보낸 여자를 안다.',
    zho: '我覺得楊毅認識那個她上個星期送出這張支票的女人。',
    cond: 'cond4',
    ...sr26
};
var sr26_cond5 = {
    eng: 'Mike knows the woman that I think she sent this check',
    kor: '서준이는 지난주에 그녀가 이 수표를 보냈다고 내가 생각하는 여자를 안다.',
    zho: '楊毅認識那個我覺得她上個星期送出這張支票的女人。',
    cond: 'cond5',
    ...sr26
};
var sr26_cond6 = {
    eng: 'Mike knows the woman that I wonder which check she sent',
    kor: '서준이는 지난주에 그녀가 어느 수표를 보냈는지 내가 궁금해하는 여자를 안다.',
    zho: '楊毅認識那個我好奇她上個星期送出哪張支票的女人。',
    cond: 'cond6',
    ...sr26
};

var sr27 = {
    type: 'critical',
    item: 'item27',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_center: 'submitted',
    choices1: jsPsych.randomization.shuffle(['Judy', 'I', 'the man', 'the application']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Judy', 'me', 'the man', 'the application']),
    correct2: 'the application'
};
var sr27_cond1 = {
    eng: 'I think Judy knows the man that submitted this application',
    kor: '내가 생각하기에 지은이는 지난주에 이 신청서를 제출한 남자를 안다.',
    zho: '我覺得周靜認識那個上個星期提交這個申請的男人。',
    cond: 'cond1',
    ...sr27
};
var sr27_cond2 = {
    eng: 'Judy knows the man that I think submitted this application',
    kor: '지은이는 지난주에 이 신청서를 제출했다고 내가 생각하는 남자를 안다.',
    zho: '周靜認識那個我覺得上個星期提交這個申請的男人。',
    cond: 'cond2',
    ...sr27
};
var sr27_cond3 = {
    eng: 'Judy knows the man that I wonder which application submitted',
    kor: '지은이는 지난주에 어느 신청서를 제출했는지 내가 궁금해하는 남자를 안다.',
    zho: '周靜認識那個我好奇上個星期提交哪個申請的男人。',
    cond: 'cond3',
    ...sr27
};
var sr27_cond4 = {
    eng: 'I think Judy knows the man that he submitted this application',
    kor: '내가 생각하기에 지은이는 지난주에 그가 이 신청서를 제출한 남자를 안다.',
    zho: '我覺得周靜認識那個他上個星期提交這個申請的男人',
    cond: 'cond4',
    ...sr27
};
var sr27_cond5 = {
    eng: 'Judy knows the man that I think he submitted this application',
    kor: '지은이는 지난주에 그가 이 신청서를 제출했다고 내가 생각하는 남자를 안다.',
    zho: '周靜認識那個我覺得他上個星期提交這個申請的男人。',
    cond: 'cond5',
    ...sr27
};
var sr27_cond6 = {
    eng: 'Judy knows the man that I wonder which application he submitted',
    kor: '지은이는 지난주에 그가 어느 신청서를 제출했는지 내가 궁금해하는 남자를 안다.',
    zho: '周靜認識那個我好奇他上個星期提交哪個申請的男人。',
    cond: 'cond6',
    ...sr27
};

var sr28 = {
    type: 'critical',
    item: 'item28',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_center: 'submitted',
    choices1: jsPsych.randomization.shuffle(['Luke', 'I', 'the woman', 'the proposal']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['Luke', 'me', 'the woman', 'the proposal']),
    correct2: 'the proposal'
};
var sr28_cond1 = {
    eng: 'I think Luke knows the woman that submitted this proposal',
    kor: '내가 생각하기에 현우는 지난주에 이 제안서를 제출한 여자를 안다.',
    zho: '我覺得陳傑認識那個上個星期提交這個提案的女人。',
    cond: 'cond1',
    ...sr28
};
var sr28_cond2 = {
    eng: 'Luke knows the woman that I think submitted this proposal',
    kor: '현우는 지난주에 이 제안서를 제출했다고 내가 생각하는 여자를 안다.',
    zho: '陳傑認識那個我覺得上個星期提交這個提案的女人。',
    cond: 'cond2',
    ...sr28
};
var sr28_cond3 = {
    eng: 'Luke knows the woman that I wonder which proposal submitted',
    kor: '현우는 지난주에 어느 제안서를 제출했는지 내가 궁금해하는 여자를 안다.',
    zho: '陳傑認識那個我好奇上個星期提交哪個提案的女人。',
    cond: 'cond3',
    ...sr28
};
var sr28_cond4 = {
    eng: 'I think Luke knows the woman that she submitted this proposal',
    kor: '내가 생각하기에 현우는 지난주에 그녀가 이 제안서를 제출한 여자를 안다.',
    zho: '我覺得陳傑認識那個她上個星期提交這個提案的女人。',
    cond: 'cond4',
    ...sr28
};
var sr28_cond5 = {
    eng: 'Luke knows the woman that I think she submitted this proposal',
    kor: '현우는 지난주에 그녀가 이 제안서를 제출했다고 내가 생각하는 여자를 안다.',
    zho: '陳傑認識那個我覺得她上個星期提交這個提案的女人。',
    cond: 'cond5',
    ...sr28
};
var sr28_cond6 = {
    eng: 'Luke knows the woman that I wonder which proposal she submitted',
    kor: '현우는 지난주에 그녀가 어느 제안서를 제출했는지 내가 궁금해하는 여자를 안다.',
    zho: '陳傑認識那個我好奇她上個星期提交哪個提案的女人。',
    cond: 'cond6',
    ...sr28
};

var sr29 = {
    type: 'critical',
    item: 'item29',
    coda: ' at the beginning of the night.',
    time: ' last night.',
    text_center: 'wore',
    choices1: jsPsych.randomization.shuffle(['Anna', 'I', 'the man', 'the coat']),
    correct1: 'the man',
    choices2: jsPsych.randomization.shuffle(['Anna', 'me', 'the man', 'the coat']),
    correct2: 'the coat'
};
var sr29_cond1 = {
    eng: 'I think Anna knows the man that wore this coat',
    kor: '내가 생각하기에 하은이는 어젯밤에 이 코트를 입은 남자를 안다.',
    zho: '我覺得王芳認識那個昨晚穿這件外套的男人。',
    cond: 'cond1',
    ...sr29
};
var sr29_cond2 = {
    eng: 'Anna knows the man that I think wore this coat',
    kor: '하은이는 어젯밤에 이 코트를 입었다고 내가 생각하는 남자를 안다.',
    zho: '王芳認識那個我覺得昨晚穿這件外套的男人。',
    cond: 'cond2',
    ...sr29
};
var sr29_cond3 = {
    eng: 'Anna knows the man that I wonder which coat wore',
    kor: '하은이는 어젯밤에 어느 코트를 입었는지 내가 궁금해하는 남자를 안다.',
    zho: '王芳認識那個我好奇昨晚穿哪件外套的男人。',
    cond: 'cond3',
    ...sr29
};
var sr29_cond4 = {
    eng: 'I think Anna knows the man that he wore this coat',
    kor: '내가 생각하기에 하은이는 어젯밤에 그가 이 코트를 입은 남자를 안다.',
    zho: '我覺得王芳認識那個他昨晚穿這件外套的男人。',
    cond: 'cond4',
    ...sr29
};
var sr29_cond5 = {
    eng: 'Anna knows the man that I think he wore this coat',
    kor: '하은이는 어젯밤에 그가 이 코트를 입었다고 내가 생각하는 남자를 안다.',
    zho: '王芳認識那個我覺得他昨晚穿這件外套的男人。',
    cond: 'cond5',
    ...sr29
};
var sr29_cond6 = {
    eng: 'Anna knows the man that I wonder which coat he wore',
    kor: '하은이는 어젯밤에 그가 어느 코트를 입었는지 내가 궁금해하는 남자를 안다.',
    zho: '王芳認識那個我好奇他昨晚穿哪件外套的男人。',
    cond: 'cond6',
    ...sr29
};

var sr30 = {
    type: 'critical',
    item: 'item30',
    coda: ' at the end of the night.',
    time: ' last night.',
    text_center: 'wore',
    choices1: jsPsych.randomization.shuffle(['Dave', 'I', 'the woman', 'the dress']),
    correct1: 'the woman',
    choices2: jsPsych.randomization.shuffle(['Dave', 'me', 'the woman', 'the dress']),
    correct2: 'the dress'
};
var sr30_cond1 = {
    eng: 'I think Dave knows the woman that wore this dress',
    kor: '내가 생각하기에 도윤이는 어젯밤에 이 드레스를 입은 여자를 안다.',
    zho: '我覺得劉浩認識那個昨晚穿這條裙子的女人。',
    cond: 'cond1',
    ...sr30
};
var sr30_cond2 = {
    eng: 'Dave knows the woman that I think wore this dress',
    kor: '도윤이는 어젯밤에 이 드레스를 입었다고 내가 생각하는 여자를 안다.',
    zho: '劉浩認識那個我覺得昨晚穿這條裙子的女人。',
    cond: 'cond2',
    ...sr30
};
var sr30_cond3 = {
    eng: 'Dave knows the woman that I wonder which dress wore',
    kor: '도윤이는 어젯밤에 어느 드레스를 입었는지 내가 궁금해하는 여자를 안다.',
    zho: '劉浩認識那個我好奇昨晚穿哪條裙子的女人。',
    cond: 'cond3',
    ...sr30
};
var sr30_cond4 = {
    eng: 'I think Dave knows the woman that she wore this dress',
    kor: '내가 생각하기에 도윤이는 어젯밤에 그녀가 이 드레스를 입은 여자를 안다.',
    zho: '我覺得劉浩認識那個她昨晚穿這條裙子的女人。',
    cond: 'cond4',
    ...sr30
};
var sr30_cond5 = {
    eng: 'Dave knows the woman that I think she wore this dress',
    kor: '도윤이는 어젯밤에 그녀가 이 드레스를 입었다고 내가 생각하는 여자를 안다.',
    zho: '劉浩認識那個我覺得她昨晚穿這條裙子的女人。',
    cond: 'cond5',
    ...sr30
};
var sr30_cond6 = {
    eng: 'Dave knows the woman that I wonder which dress she wore',
    kor: '도윤이는 어젯밤에 그녀가 어느 드레스를 입었는지 내가 궁금해하는 여자를 안다.',
    zho: '劉浩認識那個我好奇她昨晚穿哪條裙子的女人。',
    cond: 'cond6',
    ...sr30
};

// --------------------------------------------------------
// fillers for reading tasks
// --------------------------------------------------------

// set 1

var fr01 = {
    type: 'filler',
    item: 'item31',
    set: 'set1',
    cond: 'grammatical',
    eng: 'Mary says this man saw the criminals that attacked me',
    kor: '민지가 말한 대로 이 남자는 지난주에 나를 공격한 범인들을 보았다.',
    zho: '黃麗說這個男人看到了那些上個星期襲擊我的罪犯。',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_center: 'attacked',
    choices1: jsPsych.randomization.shuffle(['Mary', 'the man', 'the criminals', 'I']),
    correct1: 'the criminals',
    choices2: jsPsych.randomization.shuffle(['Mary', 'the man', 'the criminals', 'me']),
    correct2: 'me'
};

var fr02 = {
    type: 'filler',
    item: 'item32',
    set: 'set1',
    cond: 'grammatical',
    eng: 'Bill says this woman met the protesters that criticized me',
    kor: '우진이가 말한 대로 이 여자는 지난주에 나를 비판한 시위대들을 만났다.',
    zho: '吳強說這個女人遇到了那些上個星期批評我的抗議者。',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_center: 'criticized',
    choices1: jsPsych.randomization.shuffle(['Bill', 'the woman', 'the protesters', 'I']),
    correct1: 'the protesters',
    choices2: jsPsych.randomization.shuffle(['Bill', 'the woman', 'the protesters', 'me']),
    correct2: 'me'
};

var fr03 = {
    type: 'filler',
    item: 'item33',
    set: 'set1',
    cond: 'grammatical',
    eng: 'Tina says this man saw the politicians that greeted me',
    kor: '수아가 말한 대로 이 남자는 어젯밤에 나를 인사한 정치인들을 보았다.',
    zho: '趙悅說這個男人看到了那些昨晚招待我的政治家。',
    coda: ' at the end of the night.',
    time: ' last night.',
    text_center: 'greeted',
    choices1: jsPsych.randomization.shuffle(['Tina', 'the man', 'the politicians', 'I']),
    correct1: 'the politicians',
    choices2: jsPsych.randomization.shuffle(['Tina', 'the man', 'the politicians', 'me']),
    correct2: 'me'
};

var fr04 = {
    type: 'filler',
    item: 'item34',
    set: 'set1',
    cond: 'grammatical',
    eng: 'Luke says this woman met the activists that sued me',
    kor: '현우가 말한 대로 이 여자는 작년에 나를 고소한 운동가들을 만났다.',
    zho: '陳傑說這個女人遇到了那些去年起訴我的社運人士。',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_center: 'sued',
    choices1: jsPsych.randomization.shuffle(['Luke', 'the woman', 'the activists', 'I']),
    correct1: 'the activists',
    choices2: jsPsych.randomization.shuffle(['Luke', 'the woman', 'the activists', 'me']),
    correct2: 'me'
};

var fr05 = {
    type: 'filler',
    item: 'item35',
    set: 'set1',
    cond: 'grammatical',
    eng: 'Anna says this man saw the sailors that visited me',
    kor: '하은이가 말한 대로 이 남자는 어젯밤에 나를 방문한 선원들을 보았다.',
    zho: '王芳說這個男人看到了那些昨晚拜訪我的船員。',
    coda: ' at the beginning of the night.',
    time: ' last night.',
    text_center: 'visited',
    choices1: jsPsych.randomization.shuffle(['Anna', 'the man', 'the sailors', 'I']),
    correct1: 'the sailors',
    choices2: jsPsych.randomization.shuffle(['Anna', 'the man', 'the sailors', 'me']),
    correct2: 'me'
};

var fr06 = {
    type: 'filler',
    item: 'item36',
    set: 'set1',
    cond: 'grammatical',
    eng: 'John says this woman met the workers that threatened me',
    kor: '철수가 말한 대로 이 여자는 지난달에 나를 위협한 일꾼들을 만났다.',
    zho: '張偉說這個女人遇到了那些上個月威脅我的工人。',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_center: 'threatened',
    choices1: jsPsych.randomization.shuffle(['John', 'the woman', 'the workers', 'I']),
    correct1: 'the workers',
    choices2: jsPsych.randomization.shuffle(['John', 'the woman', 'the workers', 'me']),
    correct2: 'me'
};

var fr07 = {
    type: 'filler',
    item: 'item37',
    set: 'set1',
    cond: 'grammatical',
    eng: 'Lisa says this man saw the reporters that praised me',
    kor: '은지가 말한 대로 이 남자는 작년에 나를 칭찬한 기자들을 보았다.',
    zho: '李娜說這個男人看到了那些去年表揚我的記者。',
    coda: ' at the end of the year.',
    time: ' last year.',
    text_center: 'praised',
    choices1: jsPsych.randomization.shuffle(['Lisa', 'the man', 'the reporters', 'I']),
    correct1: 'the reporters',
    choices2: jsPsych.randomization.shuffle(['Lisa', 'the man', 'the reporters', 'me']),
    correct2: 'me'
};

// set 2

var fr08 = {
    type: 'filler',
    item: 'item38',
    set: 'set2',
    cond: 'ungrammatical',
    eng: 'This woman says John met the protesters that were attacked me',
    kor: '이 여자가 말한 대로 철수는 지난주에 나를 공격된 시위대들을 만났다.',
    zho: '這個女人說張偉遇到了那些上個星期被襲擊我的抗議者。',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_center: 'met',
    choices1: jsPsych.randomization.shuffle(['the woman', 'John', 'the protesters', 'I']),
    correct1: 'John',
    choices2: jsPsych.randomization.shuffle(['the woman', 'John', 'the protesters', 'me']),
    correct2: 'the protesters'
};

var fr09 = {
    type: 'filler',
    item: 'item39',
    set: 'set2',
    cond: 'ungrammatical',
    eng: 'This man says Lisa saw the reporters that were criticized me',
    kor: '이 남자가 말한 대로 은지는 지난주에 나를 비판된 기자들을 보았다.',
    zho: '這個男人說李娜看到了那些上個星期被批評我的記者。',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_center: 'saw',
    choices1: jsPsych.randomization.shuffle(['the man', 'Lisa', 'the reporters', 'I']),
    correct1: 'Lisa',
    choices2: jsPsych.randomization.shuffle(['the man', 'Lisa', 'the reporters', 'me']),
    correct2: 'the reporters'
};

var fr10 = {
    type: 'filler',
    item: 'item40',
    set: 'set2',
    cond: 'ungrammatical',
    eng: 'This woman says Mike met the sailors that were greeted me',
    kor: '이 여자가 말한 대로 서준이는 어젯밤에 나를 인사된 선원들을 만났다.',
    zho: '這個女人說楊毅遇到了那些昨晚被招待我的船員。',
    coda: ' at the beginning of the night.',
    time: ' last night.',
    text_center: 'met',
    choices1: jsPsych.randomization.shuffle(['the woman', 'Mike', 'the sailors', 'I']),
    correct1: 'Mike',
    choices2: jsPsych.randomization.shuffle(['the woman', 'Mike', 'the sailors', 'me']),
    correct2: 'the sailors'
};

var fr11 = {
    type: 'filler',
    item: 'item41',
    set: 'set2',
    cond: 'ungrammatical',
    eng: 'This man says Judy saw the workers that were sued me',
    kor: '이 남자가 말한 대로 지은이는 작년에 나를 고소된 일꾼들을 보았다.',
    zho: '這個男人說周靜看到了那些去年被起訴我的工人。',
    coda: ' at the end of the year.',
    time: ' last year.',
    text_center: 'saw',
    choices1: jsPsych.randomization.shuffle(['the man', 'Judy', 'the workers', 'I']),
    correct1: 'Judy',
    choices2: jsPsych.randomization.shuffle(['the man', 'Judy', 'the workers', 'me']),
    correct2: 'the workers'
};

var fr12 = {
    type: 'filler',
    item: 'item42',
    set: 'set2',
    cond: 'ungrammatical',
    eng: 'This woman says Dave met the activists that were visited me',
    kor: '이 여자가 말한 대로 도윤이는 어젯밤에 나를 방문된 운동가들을 만났다.',
    zho: '這個女人說劉浩遇到了那些昨晚被拜訪我的社運人士。',
    coda: ' at the end of the night.',
    time: ' last night.',
    text_center: 'met',
    choices1: jsPsych.randomization.shuffle(['the woman', 'Dave', 'the activists', 'I']),
    correct1: 'Dave',
    choices2: jsPsych.randomization.shuffle(['the woman', 'Dave', 'the activists', 'me']),
    correct2: 'the activists'
};

var fr13 = {
    type: 'filler',
    item: 'item43',
    set: 'set2',
    cond: 'ungrammatical',
    eng: 'This man says Mary saw the criminals that were threatened me',
    kor: '이 남자가 말한 대로 민지는 지난달에 나를 위협된 범인들을 보았다.',
    zho: '這個男人說黃麗看到了那些上個月被威脅我的罪犯。',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_center: 'saw',
    choices1: jsPsych.randomization.shuffle(['the man', 'Mary', 'the criminals', 'I']),
    correct1: 'Mary',
    choices2: jsPsych.randomization.shuffle(['the man', 'Mary', 'the criminals', 'me']),
    correct2: 'the criminals'
};

var fr14 = {
    type: 'filler',
    item: 'item44',
    set: 'set2',
    cond: 'ungrammatical',
    eng: 'This woman says Bill met the politicians that were praised me',
    kor: '이 여자가 말한 대로 우진이는 작년에 나를 칭찬된 정치인들을 만났다.',
    zho: '這個女人說吳強遇到了那些去年被表揚我的政治家。',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_center: 'met',
    choices1: jsPsych.randomization.shuffle(['the woman', 'Bill', 'the politicians', 'I']),
    correct1: 'Bill',
    choices2: jsPsych.randomization.shuffle(['the woman', 'Bill', 'the politicians', 'I']),
    correct2: 'the politicians'
};

// set 3

var fr15 = {
    type: 'filler',
    item: 'item45',
    set: 'set3',
    cond: 'grammatical',
    eng: 'This woman says John likes the cake that I baked',
    kor: '이 여자가 말한 대로 철수는 어젯밤에 내가 구운 케이크를 좋아한다.',
    zho: '這個女人說張偉喜歡那個我昨晚烤的蛋糕。',
    coda: ' at the beginning of the night.',
    time: ' last night.',
    text_center: 'likes',
    choices1: jsPsych.randomization.shuffle(['the woman', 'John', 'the cake', 'I']),
    correct1: 'John',
    choices2: jsPsych.randomization.shuffle(['the woman', 'John', 'the cake', 'me']),
    correct2: 'the cake'
};

var fr16 = {
    type: 'filler',
    item: 'item46',
    set: 'set3',
    cond: 'grammatical',
    eng: 'This man says Lisa found the documentary that I watched',
    kor: '이 남자 말한 대로 은지는 지난주에 내가 본 다큐멘터리를 찾아냈다.',
    zho: '這個男人說李娜找到了那個我上個星期看的紀錄片。',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_center: 'found',
    choices1: jsPsych.randomization.shuffle(['the man', 'Lisa', 'the documentary', 'I']),
    correct1: 'Lisa',
    choices2: jsPsych.randomization.shuffle(['the man', 'Lisa', 'the documentary', 'me']),
    correct2: 'the documentary'
};

var fr17 = {
    type: 'filler',
    item: 'item47',
    set: 'set3',
    cond: 'grammatical',
    eng: 'This woman says Mike likes the truck that I drove',
    kor: '이 여자 말한 대로 서준이는 지난달에 내가 운전한 트럭을 좋아한다.',
    zho: '這個女人說楊毅喜歡那輛我上個月開的卡車。',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_center: 'likes',
    choices1: jsPsych.randomization.shuffle(['the woman', 'Mike', 'the truck', 'I']),
    correct1: 'Mike',
    choices2: jsPsych.randomization.shuffle(['the woman', 'Mike', 'the truck', 'me']),
    correct2: 'the truck'
};

var fr18 = {
    type: 'filler',
    item: 'item48',
    set: 'set3',
    cond: 'grammatical',
    eng: 'This man says Judy found the store that I opened',
    kor: '이 남자가 말한 대로 지은이는 지난달에 내가 개업한 가게를 찾아냈다.',
    zho: '這個男人說周靜找到了那家我上個月開的店。',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_center: 'found',
    choices1: jsPsych.randomization.shuffle(['the man', 'Judy', 'the store', 'I']),
    correct1: 'Judy',
    choices2: jsPsych.randomization.shuffle(['the man', 'Judy', 'the store', 'me']),
    correct2: 'the store'
};

var fr19 = {
    type: 'filler',
    item: 'item49',
    set: 'set3',
    cond: 'grammatical',
    eng: 'This woman says Dave likes the magazine that I read',
    kor: '이 여자가 말한 대로 도윤이는 지난주에 내가 읽은 잡지를 좋아한다.',
    zho: '這個女人說劉浩喜歡那本我上個星期讀的雜誌。',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_center: 'likes',
    choices1: jsPsych.randomization.shuffle(['the woman', 'Dave', 'the magazine', 'I']),
    correct1: 'Dave',
    choices2: jsPsych.randomization.shuffle(['the woman', 'Dave', 'the magazine', 'me']),
    correct2: 'the magazine'
};

var fr20 = {
    type: 'filler',
    item: 'item50',
    set: 'set3',
    cond: 'grammatical',
    eng: 'This man says Tina found the article that I published',
    kor: '이 남자가 말한 대로 수아는 작년에 내가 출판한 기사를 찾아냈다.',
    zho: '這個男人說趙悅找到了那篇我去年發表的文章。',
    coda: ' at the end of the year.',
    time: ' last year.',
    text_center: 'found',
    choices1: jsPsych.randomization.shuffle(['the man', 'Tina', 'the article', 'I']),
    correct1: 'Tina',
    choices2: jsPsych.randomization.shuffle(['the man', 'Tina', 'the article', 'me']),
    correct2: 'the article'
};

var fr21 = {
    type: 'filler',
    item: 'item51',
    set: 'set3',
    cond: 'grammatical',
    eng: 'This woman says Luke likes the food that I cooked',
    kor: '이 여자가 말한 대로 현우는 어젯밤에 내가 요리한 음식을 좋아한다.',
    zho: '這個女人說陳傑喜歡那碗我昨晚煮的麵。', 
    coda: ' at the end of the night.',
    time: ' last night.',
    text_center: 'likes',
    choices1: jsPsych.randomization.shuffle(['the woman', 'Luke', 'the food', 'I']),
    correct1: 'Luke',
    choices2: jsPsych.randomization.shuffle(['the woman', 'Luke', 'the food', 'me']),
    correct2: 'the food'
};

// set 4

var fr22 = {
    type: 'filler',
    item: 'item52',
    set: 'set4',
    cond: 'ungrammatical',
    eng: 'Mary says this man found cookie that I baked',
    kor: '민지가 말한 대로 이 남자는 어젯밤에 내가 구운 쿠키가 찾아냈다.',
    zho: '黃麗說這個男人找到了那我昨晚烤餅乾。',
    coda: ' at the end of the night.',
    time: ' last night.',
    text_center: 'baked',
    choices1: jsPsych.randomization.shuffle(['Mary', 'the man', 'the cookie', 'I']),
    correct1: 'I',
    choices2: jsPsych.randomization.shuffle(['Mary', 'the man', 'the cookie', 'me']),
    correct2: 'the cookie'
};

var fr23 = {
    type: 'filler',
    item: 'item53',
    set: 'set4',
    cond: 'ungrammatical',
    eng: 'Bill says this woman likes play that I watched',
    kor: '우진이가 말한 대로 이 여자는 지난주에 내가 본 연극이 좋아한다.',
    zho: '吳強說這個女人喜歡那我上個星期看戲。',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_center: 'watched',
    choices1: jsPsych.randomization.shuffle(['Bill', 'the woman', 'the play', 'I']),
    correct1: 'I',
    choices2: jsPsych.randomization.shuffle(['Bill', 'the woman', 'the play', 'me']),
    correct2: 'the play'
};

var fr24 = {
    type: 'filler',
    item: 'item54',
    set: 'set4',
    cond: 'ungrammatical',
    eng: 'Tina says this man found bus that I drove',
    kor: '수아가 말한 대로 이 남자는 지난달에 내가 운전한 버스가 찾아냈다.',
    zho: '趙悅說這個男人找到了那我上個月開公共汽車。',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_center: 'drove',
    choices1: jsPsych.randomization.shuffle(['Tina', 'the man', 'the bus', 'I']),
    correct1: 'I',
    choices2: jsPsych.randomization.shuffle(['Tina', 'the man', 'the bus', 'me']),
    correct2: 'the bus'
};

var fr25 = {
    type: 'filler',
    item: 'item55',
    set: 'set4',
    cond: 'ungrammatical',
    eng: 'Luke says this woman likes shop that I opened',
    kor: '현우가 말한 대로 이 여자는 지난달에 내가 개업한 점포가 좋아한다.',
    zho: '陳傑說這個女人喜歡那我上個月開店鋪。',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_center: 'opened',
    choices1: jsPsych.randomization.shuffle(['Luke', 'the woman', 'the shop', 'I']),
    correct1: 'I',
    choices2: jsPsych.randomization.shuffle(['Luke', 'the woman', 'the shop', 'me']),
    correct2: 'the shop'
};

var fr26 = {
    type: 'filler',
    item: 'item56',
    set: 'set4',
    cond: 'ungrammatical',
    eng: 'Anna says this man found newspaper that I read',
    kor: '하은이가 말한 대로 이 남자는 지난주에 내가 읽은 신문이 찾아냈다.',
    zho: '王芳說這個男人找到了那我上個星期讀報紙。',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_center: 'read',
    choices1: jsPsych.randomization.shuffle(['Anna', 'the man', 'the newspaper', 'I']),
    correct1: 'I',
    choices2: jsPsych.randomization.shuffle(['Anna', 'the man', 'the newspaper', 'me']),
    correct2: 'the newspaper'
};

var fr27 = {
    type: 'filler',
    item: 'item57',
    set: 'set4',
    cond: 'ungrammatical',
    eng: 'Mike says this woman likes essay that I published',
    kor: '서준이가 말한 대로 이 여자는 작년에 내가 출판한 에세이가 좋아한다.',
    zho: '楊毅說這個女人喜歡那我去年發表論文。',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_center: 'published',
    choices1: jsPsych.randomization.shuffle(['Mike', 'the woman', 'the essay', 'I']),
    correct1: 'I',
    choices2: jsPsych.randomization.shuffle(['Mike', 'the woman', 'the essay', 'me']),
    correct2: 'the essay'
};

var fr28 = {
    type: 'filler',
    item: 'item58',
    set: 'set4',
    cond: 'ungrammatical',
    eng: 'Judy says this man found meal that I cooked',
    kor: '지은이가 말한 대로 이 남자는 어젯밤에 내가 요리한 식사가 찾아냈다.',
    zho: '周靜說這個男人找到了那我昨晚煮飯。',
    coda: ' at the beginning of the night.',
    time: ' last night.',
    text_center: 'cooked',
    choices1: jsPsych.randomization.shuffle(['Judy', 'the man', 'the meal', 'I']),
    correct1: 'I',
    choices2: jsPsych.randomization.shuffle(['Judy', 'the man', 'the meal', 'me']),
    correct2: 'the meal'
};

// set 5

var fr29 = {
    type: 'filler',
    item: 'item59',
    set: 'set5',
    cond: 'grammatical',
    eng: 'This man wonders who met the lifeguards that helped Tina',
    kor: '이 남자는 지난주에 수아를 도와준 구조대원들을 누가 만났는지 궁금해한다.',
    zho: '這個男人好奇誰遇到了那些上個星期幫助趙悅的救生員們。',
    coda: ' at the beginning of the week.',
    time: ' last week.',
    text_left: 'Someone helped',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['the firefighters', 'the lifeguards', 'Tina', 'Judy']),
    correct1: 'Tina',
    choices2: jsPsych.randomization.shuffle(['beginning of the week', 'end of the week', 'beginning of the night', 'end of the night']),
    correct2: 'beginning of the week'
};

var fr30 = {
    type: 'filler',
    item: 'item60',
    set: 'set5',
    cond: 'grammatical',
    eng: 'This woman wonders who saw the firefighters that helped Dave',
    kor: '이 여자는 지난주에 도윤이를 도와준 소방관들을 누가 보았는지 궁금해한다.',
    zho: '這個女人好奇誰遇到了那些上個星期幫助劉浩的消防員們。',
    coda: ' at the end of the week.',
    time: ' last week.',
    text_left: 'Someone helped',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['the lifeguards', 'the firefighters', 'Dave', 'John']),
    correct1: 'Dave',
    choices2: jsPsych.randomization.shuffle(['end of the week', 'beginning of the week', 'end of the night', 'beginning of the night']),
    correct2: 'end of the week'
};

var fr31 = {
    type: 'filler',
    item: 'item61',
    set: 'set5',
    cond: 'grammatical',
    eng: 'This man wonders who met the firefighters that saved Lisa',
    kor: '이 남자는 어젯밤에 은지를 구해준 소방관들을 누가 만났는지 궁금해한다.',
    zho: '這個男人好奇誰遇到了那些昨晚救了李娜的消防員們。',
    coda: ' at the end of the night.',
    time: ' last night.',
    text_left: 'Someone saved',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['the lifeguards', 'the firefighters', 'Lisa', 'Tina']),
    correct1: 'Lisa',
    choices2: jsPsych.randomization.shuffle(['end of the night', 'beginning of the night', 'end of the week', 'beginning of the week']),
    correct2: 'end of the night'
};

var fr32 = {
    type: 'filler',
    item: 'item62',
    set: 'set5',
    cond: 'grammatical',
    eng: 'This woman wonders who saw the lifeguards that saved John',
    kor: '이 여자는 어젯밤에 철수를 구해준 구조대원들을 누가 보았는지 궁금해한다.',
    zho: '這個女人好奇誰遇到了那些昨晚救了張偉的救生員們。',
    coda: ' at the beginning of the night.',
    time: ' last night.',
    text_left: 'Someone saved',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['the firefighters', 'the lifeguards', 'John', 'Bill']),
    correct1: 'John',
    choices2: jsPsych.randomization.shuffle(['beginning of the night', 'end of the night', 'beginning of the week', 'end of the week']),
    correct2: 'beginning of the night'
};

var fr33 = {
    type: 'filler',
    item: 'item63',
    set: 'set5',
    cond: 'grammatical',
    eng: 'This man wonders who met the students that teased Mary',
    kor: '이 남자는 지난달에 민지를 놀린 학생들을 누가 만났는지 궁금해한다.',
    zho: '這個男人好奇誰遇到了那些上個月取笑黃麗的學生們。',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_left: 'Someone teased',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['the journalists', 'the students', 'Mary', 'Lisa']),
    correct1: 'Mary',
    choices2: jsPsych.randomization.shuffle(['beginning of the month', 'end of the month', 'beginning of the year', 'end of the year']),
    correct2: 'beginning of the month'
};

var fr34 = {
    type: 'filler',
    item: 'item64',
    set: 'set5',
    cond: 'grammatical',
    eng: 'This woman wonders who saw the journalists that teased Bill',
    kor: '이 여자는 지난달에 우진이를 놀린 언론인들을 누가 보았는지 궁금해한다.',
    zho: '這個女人好奇誰遇到了那些上個月取笑吳強的記者們。',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_left: 'Someone teased',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['the students', 'the journalists', 'Bill', 'Mike']),
    correct1: 'Bill',
    choices2: jsPsych.randomization.shuffle(['end of the month', 'beginning of the month', 'end of the year', 'beginning of the year']),
    correct2: 'end of the month'
};

var fr35 = {
    type: 'filler',
    item: 'item65',
    set: 'set5',
    cond: 'grammatical',
    eng: 'This man wonders who met the journalists that tricked Anna',
    kor: '이 남자는 작년에 하은이를 속인 언론인들을 누가 만났는지 궁금해한다.',
    zho: '這個男人好奇誰遇到了那些去年欺騙王芳的記者們。',
    coda: ' at the end of the year.',
    time: ' last year.',
    text_left: 'Someone tricked',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['the students', 'the journalists', 'Anna', 'Mary']),
    correct1: 'Anna',
    choices2: jsPsych.randomization.shuffle(['end of the year', 'beginning of the year', 'end of the month', 'beginning of the month']),
    correct2: 'end of the year'
};

// set 6

var fr36 = {
    type: 'filler',
    item: 'item66',
    set: 'set6',
    cond: 'grammatical',
    eng: 'John thinks this woman found the report that he prepared',
    kor: '철수가 생각하기에 이 여자는 지난달에 그가 준비한 보고서를 찾아냈다.',
    zho: '張偉覺得這個女人找到了那份他上個月準備的報告。',
    coda: ' at the end of the month.',
    time: ' last month.',
    text_left: 'Someone prepared',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['John', 'Bill', 'the report', 'the presentation']),
    correct1: 'the report',
    choices2: jsPsych.randomization.shuffle(['end of the month', 'beginning of the month', 'end of the year', 'beginning of the year']),
    correct2: 'end of the month'
};

var fr37 = {
    type: 'filler',
    item: 'item67',
    set: 'set6',
    cond: 'grammatical',
    eng: 'Judy thinks this man likes the movie that she directed',
    kor: '지은이가 생각하기에 이 남자는 작년에 그녀가 감독한 영화를 좋아한다.',
    zho: '周靜覺得這個男人喜歡那部她去年導演的電影。',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_left: 'Someone directed',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['Judy', 'Anna', 'the movie', 'the show']),
    correct1: 'the movie',
    choices2: jsPsych.randomization.shuffle(['beginning of the year', 'end of the year', 'beginning of the month', 'end of the month']),
    correct2: 'beginning of the year'
};

var fr38 = {
    type: 'filler',
    item: 'item68',
    set: 'set6',
    cond: 'grammatical',
    eng: 'Luke thinks this woman likes the show that he directed',
    kor: '현우가 생각하기에 이 여자는 작년에 그가 감독한 드라마를 좋아한다.',
    zho: '陳傑覺得這個女人喜歡那個他去年監製的節目。',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_left: 'Someone directed',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['Luke', 'Dave', 'the show', 'the movie']),
    correct1: 'the show',
    choices2: jsPsych.randomization.shuffle(['beginning of the year', 'end of the year', 'beginning of the month', 'end of the month']),
    correct2: 'beginning of the year'
};

var fr39 = {
    type: 'filler',
    item: 'item69',
    set: 'set6',
    cond: 'grammatical',
    eng: 'Anna thinks this man likes the story that she told',
    kor: '하은이가 생각하기에 이 남자는 어젯밤에 그녀가 한 이야기를 좋아한다.',
    zho: '王芳覺得這個男人喜歡那個她昨晚講的故事。',
    coda: ' at the end of the night.',
    time: ' last night.',
    text_left: 'Someone told',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['Anna', 'Mary', 'the story', 'the joke']),
    correct1: 'the story',
    choices2: jsPsych.randomization.shuffle(['end of the night', 'beginning of the night', 'end of the week', 'beginning of the week']),
    correct2: 'end of the night'
};

var fr40 = {
    type: 'filler',
    item: 'item70',
    set: 'set6',
    cond: 'grammatical',
    eng: 'Dave thinks this woman likes the joke that he told',
    kor: '도윤이가 생각하기에 이 여자는 어젯밤에 그가 한 농담을 좋아한다.',
    zho: '劉浩覺得這個女人喜歡那個他昨晚講的笑話。',
    coda: ' at the end of the night.',
    time: ' last night.',
    text_left: 'Someone told',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['Dave', 'John', 'the joke', 'the story']),
    correct1: 'the joke',
    choices2: jsPsych.randomization.shuffle(['end of the night', 'beginning of the night', 'end of the week', 'beginning of the week']),
    correct2: 'end of the night'
};

var fr41 = {
    type: 'filler',
    item: 'item71',
    set: 'set6',
    cond: 'grammatical',
    eng: 'Mary thinks this man found the novel that she wrote',
    kor: '민지가 생각하기에 이 남자는 작년에 그녀가 쓴 소설을 찾아냈다.',
    zho: '黃麗覺得這個男人找到了那部她去年寫的小說。',
    coda: ' at the beginning of the year.',
    time: ' last year.',
    text_left: 'Someone wrote',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['Mary', 'Lisa', 'the novel', 'the poem']),
    correct1: 'the novel',
    choices2: jsPsych.randomization.shuffle(['beginning of the year', 'end of the year', 'beginning of the month', 'end of the month']),
    correct2: 'beginning of the year'
};

var fr42 = {
    type: 'filler',
    item: 'item72',
    set: 'set6',
    cond: 'grammatical',
    eng: 'Mike thinks this woman found the poem that he wrote',
    kor: '서준이가 생각하기에 이 여자는 지난달에 그가 쓴 시를 찾아냈다.',
    zho: '楊毅覺得這個女人找到了那首他上個月寫的詩。',
    coda: ' at the beginning of the month.',
    time: ' last month.',
    text_left: '',
    text_center: 'was written at the',
    choices1: jsPsych.randomization.shuffle(['Mike', 'Luke', 'the poem', 'the novel']),
    correct1: 'the poem',
    choices2: jsPsych.randomization.shuffle(['beginning of the month', 'end of the month', 'beginning of the year', 'end of the year']),
    correct2: 'beginning of the month'
};

// --------------------------------------------------------
// running lists for reading tasks
// --------------------------------------------------------

// fillers

var rfill = [
    fr01, fr02, fr03, fr04, fr05, fr06, fr07,
    fr08, fr09, fr10, fr11, fr12, fr13, fr14,
    fr15, fr16, fr17, fr18, fr19, fr20, fr21,
    fr22, fr23, fr24, fr25, fr26, fr27, fr28,
    fr29, fr30, fr31, fr32, fr33, fr34, fr35,
    fr36, fr37, fr38, fr39, fr40, fr41, fr42
];

// DO study

var drx1 = [
    dr01_cond1, dr02_cond2, dr03_cond3, dr04_cond4, dr05_cond5, dr06_cond6,
    dr07_cond1, dr08_cond2, dr09_cond3, dr10_cond4, dr11_cond5, dr12_cond6,
    dr13_cond1, dr14_cond2, dr15_cond3, dr16_cond4, dr17_cond5, dr18_cond6,
    dr19_cond1, dr20_cond2, dr21_cond3, dr22_cond4, dr23_cond5, dr24_cond6,
    dr25_cond1, dr26_cond2, dr27_cond3, dr28_cond4, dr29_cond5, dr30_cond6
];

var drx2 = [
    dr01_cond2, dr02_cond3, dr03_cond4, dr04_cond5, dr05_cond6, dr06_cond1,
    dr07_cond2, dr08_cond3, dr09_cond4, dr10_cond5, dr11_cond6, dr12_cond1,
    dr13_cond2, dr14_cond3, dr15_cond4, dr16_cond5, dr17_cond6, dr18_cond1,
    dr19_cond2, dr20_cond3, dr21_cond4, dr22_cond5, dr23_cond6, dr24_cond1,
    dr25_cond2, dr26_cond3, dr27_cond4, dr28_cond5, dr29_cond6, dr30_cond1
];

var drx3 = [
    dr01_cond3, dr02_cond4, dr03_cond5, dr04_cond6, dr05_cond1, dr06_cond2,
    dr07_cond3, dr08_cond4, dr09_cond5, dr10_cond6, dr11_cond1, dr12_cond2,
    dr13_cond3, dr14_cond4, dr15_cond5, dr16_cond6, dr17_cond1, dr18_cond2,
    dr19_cond3, dr20_cond4, dr21_cond5, dr22_cond6, dr23_cond1, dr24_cond2,
    dr25_cond3, dr26_cond4, dr27_cond5, dr28_cond6, dr29_cond1, dr30_cond2
];

var drx4 = [
    dr01_cond4, dr02_cond5, dr03_cond6, dr04_cond1, dr05_cond2, dr06_cond3,
    dr07_cond4, dr08_cond5, dr09_cond6, dr10_cond1, dr11_cond2, dr12_cond3,
    dr13_cond4, dr14_cond5, dr15_cond6, dr16_cond1, dr17_cond2, dr18_cond3,
    dr19_cond4, dr20_cond5, dr21_cond6, dr22_cond1, dr23_cond2, dr24_cond3,
    dr25_cond4, dr26_cond5, dr27_cond6, dr28_cond1, dr29_cond2, dr30_cond3
];

var drx5 = [
    dr01_cond5, dr02_cond6, dr03_cond1, dr04_cond2, dr05_cond3, dr06_cond4,
    dr07_cond5, dr08_cond6, dr09_cond1, dr10_cond2, dr11_cond3, dr12_cond4,
    dr13_cond5, dr14_cond6, dr15_cond1, dr16_cond2, dr17_cond3, dr18_cond4,
    dr19_cond5, dr20_cond6, dr21_cond1, dr22_cond2, dr23_cond3, dr24_cond4,
    dr25_cond5, dr26_cond6, dr27_cond1, dr28_cond2, dr29_cond3, dr30_cond4
];

var drx6 = [
    dr01_cond6, dr02_cond1, dr03_cond2, dr04_cond3, dr05_cond4, dr06_cond5,
    dr07_cond6, dr08_cond1, dr09_cond2, dr10_cond3, dr11_cond4, dr12_cond5,
    dr13_cond6, dr14_cond1, dr15_cond2, dr16_cond3, dr17_cond4, dr18_cond5,
    dr19_cond6, dr20_cond1, dr21_cond2, dr22_cond3, dr23_cond4, dr24_cond5,
    dr25_cond6, dr26_cond1, dr27_cond2, dr28_cond3, dr29_cond4, dr30_cond5
];

var dr_test = [dr01_cond1, fr01, dr02_cond6]; // short list for testing

// SU study

var srx1 = [
    sr01_cond1, sr02_cond2, sr03_cond3, sr04_cond4, sr05_cond5, sr06_cond6,
    sr07_cond1, sr08_cond2, sr09_cond3, sr10_cond4, sr11_cond5, sr12_cond6,
    sr13_cond1, sr14_cond2, sr15_cond3, sr16_cond4, sr17_cond5, sr18_cond6,
    sr19_cond1, sr20_cond2, sr21_cond3, sr22_cond4, sr23_cond5, sr24_cond6,
    sr25_cond1, sr26_cond2, sr27_cond3, sr28_cond4, sr29_cond5, sr30_cond6
];

var srx2 = [
    sr01_cond2, sr02_cond3, sr03_cond4, sr04_cond5, sr05_cond6, sr06_cond1,
    sr07_cond2, sr08_cond3, sr09_cond4, sr10_cond5, sr11_cond6, sr12_cond1,
    sr13_cond2, sr14_cond3, sr15_cond4, sr16_cond5, sr17_cond6, sr18_cond1,
    sr19_cond2, sr20_cond3, sr21_cond4, sr22_cond5, sr23_cond6, sr24_cond1,
    sr25_cond2, sr26_cond3, sr27_cond4, sr28_cond5, sr29_cond6, sr30_cond1
];

var srx3 = [
    sr01_cond3, sr02_cond4, sr03_cond5, sr04_cond6, sr05_cond1, sr06_cond2,
    sr07_cond3, sr08_cond4, sr09_cond5, sr10_cond6, sr11_cond1, sr12_cond2,
    sr13_cond3, sr14_cond4, sr15_cond5, sr16_cond6, sr17_cond1, sr18_cond2,
    sr19_cond3, sr20_cond4, sr21_cond5, sr22_cond6, sr23_cond1, sr24_cond2,
    sr25_cond3, sr26_cond4, sr27_cond5, sr28_cond6, sr29_cond1, sr30_cond2
];

var srx4 = [
    sr01_cond4, sr02_cond5, sr03_cond6, sr04_cond1, sr05_cond2, sr06_cond3,
    sr07_cond4, sr08_cond5, sr09_cond6, sr10_cond1, sr11_cond2, sr12_cond3,
    sr13_cond4, sr14_cond5, sr15_cond6, sr16_cond1, sr17_cond2, sr18_cond3,
    sr19_cond4, sr20_cond5, sr21_cond6, sr22_cond1, sr23_cond2, sr24_cond3,
    sr25_cond4, sr26_cond5, sr27_cond6, sr28_cond1, sr29_cond2, sr30_cond3
];

var srx5 = [
    sr01_cond5, sr02_cond6, sr03_cond1, sr04_cond2, sr05_cond3, sr06_cond4,
    sr07_cond5, sr08_cond6, sr09_cond1, sr10_cond2, sr11_cond3, sr12_cond4,
    sr13_cond5, sr14_cond6, sr15_cond1, sr16_cond2, sr17_cond3, sr18_cond4,
    sr19_cond5, sr20_cond6, sr21_cond1, sr22_cond2, sr23_cond3, sr24_cond4,
    sr25_cond5, sr26_cond6, sr27_cond1, sr28_cond2, sr29_cond3, sr30_cond4
];

var srx6 = [
    sr01_cond6, sr02_cond1, sr03_cond2, sr04_cond3, sr05_cond4, sr06_cond5,
    sr07_cond6, sr08_cond1, sr09_cond2, sr10_cond3, sr11_cond4, sr12_cond5,
    sr13_cond6, sr14_cond1, sr15_cond2, sr16_cond3, sr17_cond4, sr18_cond5,
    sr19_cond6, sr20_cond1, sr21_cond2, sr22_cond3, sr23_cond4, sr24_cond5,
    sr25_cond6, sr26_cond1, sr27_cond2, sr28_cond3, sr29_cond4, sr30_cond5
];

var sr_test = [sr01_cond1, fr01, sr02_cond6]; // short list for testing

// --------------------------------------------------------
// self-paced reading practice
// --------------------------------------------------------

var spr_prac1 = {
    type: 'practice',
    item: 'prac1',
    sent: 'Everyone thinks I know the journalists that contacted John at the beginning of the year.',
    text_center: 'contacted',
    choices1: jsPsych.randomization.shuffle(['John', 'everyone', 'the journalists', 'I']),
    correct1: 'the journalists',
    choices2: jsPsych.randomization.shuffle(['John', 'everyone', 'the journalists', 'me']),
    correct2: 'John'
};

var spr_prac2 = {
    type: 'practice',
    item: 'prac2',
    sent: 'Everyone says Mary likes the man that kissed me at the end of the week.',
    text_left: 'Someone kissed',
    text_center: 'at the',
    choices1: jsPsych.randomization.shuffle(['everyone', 'me', 'Mary', 'Lisa']),
    correct1: 'me',
    choices2: jsPsych.randomization.shuffle(['end of the week', 'beginning of the week', 'end of the night', 'beginning of the night']),
    correct2: 'end of the week'
};

var spr_prac3 = {
    type: 'practice',
    item: 'prac3',
    sent: 'Tina says everyone likes the pictures that I took at the beginning of the month.',
    text_center: 'likes',
    choices1: jsPsych.randomization.shuffle(['everyone', 'Tina', 'the pictures', 'I']),
    correct1: 'everyone',
    choices2: jsPsych.randomization.shuffle(['everyone', 'Tina', 'the pictures', 'me']),
    correct2: 'the pictures'
};

var spr_prac_list = [spr_prac1, spr_prac2, spr_prac3];

// --------------------------------------------------------
// acceptability judgment practice
// --------------------------------------------------------

var ajt_prac1 = {
    eng: 'Everyone thinks I know the journalists that contacted John last year.',
    kor: '내가 작년에 철수에게 연락한 언론인들을 안다고 모두가 생각한다.',
    zho: '每個人都覺得我認識去年聯繫將張偉的記者。',
    type: 'practice',
    item: 'prac1',
    eng_cond: 'grammatical',
    kor_cond: 'grammatical',
    zho_cond: 'ungrammatical'
};

var ajt_prac2 = {
    eng: 'Everyone says Mary is likes the man that kissed me last week.',
    kor: '모두가 말한 대로 민지는 지난주에 나에게 키스한 남자를 좋아한다.',
    zho: '每個人都說黃麗喜歡上個星期吻我的男人。',
    type: 'practice',
    item: 'prac2',
    eng_cond: 'ungrammatical',
    kor_cond: 'grammatical',
    zho_cond: 'grammatical'
};

var ajt_prac3 = {
    eng: 'Tina says everyone likes the pictures that I took last month.',
    kor: '수아가 말한 대로 모두는 지난달에게 내가 찍은 사진을 좋아한다.',
    zho: '趙悅說每個人都喜歡我上個月拍的照片。',
    type: 'practice',
    item: 'prac3',
    eng_cond: 'grammatical',
    kor_cond: 'ungrammatical',
    zho_cond: 'grammatical'
};

var eng_ajt_prac_list = [ajt_prac1, ajt_prac2, ajt_prac3];

var kor_ajt_prac_list = [ajt_prac1, ajt_prac2, ajt_prac3];

var zho_ajt_prac_list = [ajt_prac1, ajt_prac2, ajt_prac3];

// --------------------------------------------------------
// DO study: critical items for production task
// --------------------------------------------------------

var dp01 = {
    type: 'critical',
    item: 'item01',
    image1: 'img_man1.png',
    image2: 'img_man2.png'
};
var dp01_cond1 = {
    setup1: 'The officers arrested this man last week.', // stimulus
    setup2: 'The detectives arrested this man last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'The detectives arrested this man last week.',
    stimulus: 'The officers arrested this man last week.',
    target: 'the man that the officers arrested last week',
    cond: 'cond1',
    ...dp01
};
var dp01_cond2 = {
    setup1: 'Mary thinks the officers arrested this man last week.', // stimulus
    setup2: 'Lisa thinks the officers arrested this man last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Lisa thinks the officers arrested this man last week.',
    stimulus: 'Mary thinks the officers arrested this man last week.',
    target: 'the man that Mary thinks the officers arrested last week',
    cond: 'cond2',
    ...dp01
};
var dp01_cond3 = {
    setup1: 'Mary wonders which officers arrested this man last week.', // stimulus
    setup2: 'Lisa wonders which officers arrested this man last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Lisa wonders which officers arrested this man last week.',
    stimulus: 'Mary wonders which officers arrested this man last week.',
    target: 'the man that Mary wonders which officers arrested him last week',
    cond: 'cond3',
    ...dp01
};

var dp02 = {
    type: 'critical',
    item: 'item02',
    image1: 'img_woman1.png',
    image2: 'img_woman2.png'
};
var dp02_cond1 = {
    setup1: 'The officials caught this woman last night.',
    setup2: 'The detectives caught this woman last night.', // stimulus
    response1: 'The officials caught this woman last night.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'The detectives caught this woman last night.',
    target: 'the woman that the detectives caught last night',
    cond: 'cond1',
    ...dp02
};
var dp02_cond2 = {
    setup1: 'Mike thinks the detectives caught this woman last night.',
    setup2: 'Bill thinks the detectives caught this woman last night.', // stimulus
    response1: 'Mike thinks the detectives caught this woman last night.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Bill thinks the detectives caught this woman last night.',
    target: 'the woman that Bill thinks the detectives caught last night',
    cond: 'cond2',
    ...dp02
};
var dp02_cond3 = {
    setup1: 'Mike wonders which detectives caught this woman last night.',
    setup2: 'Bill wonders which detectives caught this woman last night.', // stimulus
    response1: 'Mike wonders which detectives caught this woman last night.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Bill wonders which detectives caught this woman last night.',
    target: 'the woman that Bill wonders which detectives caught her last night',
    cond: 'cond3',
    ...dp02
};

var dp03 = {
    type: 'critical',
    item: 'item03',
    image1: 'img_man3.png',
    image2: 'img_man4.png'
};
var dp03_cond1 = {
    setup1: 'The lawyers defended this man last month.', // stimulus
    setup2: 'The firm defended this man last month.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'The firm defended this man last month.',
    stimulus: 'The lawyers defended this man last month.',
    target: 'the man that the lawyers defended last month',
    cond: 'cond1',
    ...dp03
};
var dp03_cond2 = {
    setup1: 'Tina thinks the lawyers defended this man last month.', // stimulus
    setup2: 'Judy thinks the lawyers defended this man last month.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Judy thinks the lawyers defended this man last month.',
    stimulus: 'Tina thinks the lawyers defended this man last month.',
    target: 'the man that Tina thinks the lawyers defended last month',
    cond: 'cond2',
    ...dp03
};
var dp03_cond3 = {
    setup1: 'Tina wonders which lawyers defended this man last month.', // stimulus
    setup2: 'Judy wonders which lawyers defended this man last month.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Judy wonders which lawyers defended this man last month.',
    stimulus: 'Tina wonders which lawyers defended this man last month.',
    target: 'the man that Tina wonders which lawyers defended him last month',
    cond: 'cond3',
    ...dp03
};

var dp04 = {
    type: 'critical',
    item: 'item04',
    image1: 'img_woman3.png',
    image2: 'img_woman4.png'
};
var dp04_cond1 = {
    setup1: 'The company employed this woman last year.',
    setup2: 'The restaurant employed this woman last year.', // stimulus
    response1: 'The company employed this woman last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'The restaurant employed this woman last year.',
    target: 'the woman that the restaurant employed last year',
    cond: 'cond1',
    ...dp04
};
var dp04_cond2 = {
    setup1: 'Dave thinks the restaurant employed this woman last year.',
    setup2: 'Luke thinks the restaurant employed this woman last year.', // stimulus
    response1: 'Dave thinks the restaurant employed this woman last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Luke thinks the restaurant employed this woman last year.',
    target: 'the woman that Luke thinks the restaurant employed last year',
    cond: 'cond2',
    ...dp04
};
var dp04_cond3 = {
    setup1: 'Dave wonders which restaurant employed this woman last year.',
    setup2: 'Luke wonders which restaurant employed this woman last year.', // stimulus
    response1: 'Dave wonders which restaurant employed this woman last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Luke wonders which restaurant employed this woman last year.',
    target: 'the woman that Luke wonders which restaurant employed her last year',
    cond: 'cond3',
    ...dp04
};

var dp05 = {
    type: 'critical',
    item: 'item05',
    image1: 'img_man2.png',
    image2: 'img_man1.png'
};
var dp05_cond1 = {
    setup1: 'The nurses examined this man last night.', // stimulus
    setup2: 'The doctors examined this man last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'The doctors examined this man last night.',
    stimulus: 'The nurses examined this man last night.',
    target: 'the man that the nurses examined last night',
    cond: 'cond1',
    ...dp05
};
var dp05_cond2 = {
    setup1: 'Anna thinks the nurses examined this man last night.', // stimulus
    setup2: 'Mary thinks the nurses examined this man last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Mary thinks the nurses examined this man last night.',
    stimulus: 'Anna thinks the nurses examined this man last night.',
    target: 'the man that Anna thinks the nurses examined last night',
    cond: 'cond2',
    ...dp05
};
var dp05_cond3 = {
    setup1: 'Anna wonders which nurses examined this man last night.', // stimulus
    setup2: 'Mary wonders which nurses examined this man last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Mary wonders which nurses examined this man last night.',
    stimulus: 'Anna wonders which nurses examined this man last night.',
    target: 'the man that Anna wonders which nurses examined him last night',
    cond: 'cond3',
    ...dp05
};

var dp06 = {
    type: 'critical',
    item: 'item06',
    image1: 'img_woman2.png',
    image2: 'img_woman1.png'
};
var dp06_cond1 = {
    setup1: 'The company fired this woman last year.',
    setup2: 'The business fired this woman last year.', // stimulus
    response1: 'The company fired this woman last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'The business fired this woman last year.',
    target: 'the woman that the business fired last year',
    cond: 'cond1',
    ...dp06
};
var dp06_cond2 = {
    setup1: 'Bill thinks the business fired this woman last year.',
    setup2: 'John thinks the business fired this woman last year.', // stimulus
    response1: 'Bill thinks the business fired this woman last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'John thinks the business fired this woman last year.',
    target: 'the woman that John thinks the business fired last year',
    cond: 'cond2',
    ...dp06
};
var dp06_cond3 = {
    setup1: 'Bill wonders which business fired this woman last year.',
    setup2: 'John wonders which business fired this woman last year.', // stimulus
    response1: 'Bill wonders which business fired this woman last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'John wonders which business fired this woman last year.',
    target: 'the woman that John wonders which business fired her last year',
    cond: 'cond3',
    ...dp06
};

var dp07 = {
    type: 'critical',
    item: 'item07',
    image1: 'img_man4.png',
    image2: 'img_man3.png'
};
var dp07_cond1 = {
    setup1: 'The company hired this man last year.', // stimulus
    setup2: 'The restaurant hired this man last year.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'The restaurant hired this man last year.',
    stimulus: 'The company hired this man last year.',
    target: 'the man that the company hired last year',
    cond: 'cond1',
    ...dp07
};
var dp07_cond2 = {
    setup1: 'Lisa thinks the company hired this man last year.', // stimulus
    setup2: 'Tina thinks the company hired this man last year.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Tina thinks the company hired this man last year.',
    stimulus: 'Lisa thinks the company hired this man last year.',
    target: 'the man that Lisa thinks the company hired last year',
    cond: 'cond2',
    ...dp07
};
var dp07_cond3 = {
    setup1: 'Lisa wonders which company hired this man last year.', // stimulus
    setup2: 'Tina wonders which company hired this man last year.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Tina wonders which company hired this man last year.',
    stimulus: 'Lisa wonders which company hired this man last year.',
    target: 'the man that Lisa wonders which company hired him last year',
    cond: 'cond3',
    ...dp07
};

var dp08 = {
    type: 'critical',
    item: 'item08',
    image1: 'img_woman4.png',
    image2: 'img_woman3.png'
};
var dp08_cond1 = {
    setup1: 'The soldiers punished this woman last year.',
    setup2: 'The agency punished this woman last year.', // stimulus
    response1: 'The soldiers punished this woman last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'The agency punished this woman last year.',
    target: 'the woman that the agency punished last year',
    cond: 'cond1',
    ...dp08
};
var dp08_cond2 = {
    setup1: 'Luke thinks the agency punished this woman last year.',
    setup2: 'Mike thinks the agency punished this woman last year.', // stimulus
    response1: 'Luke thinks the agency punished this woman last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Mike thinks the agency punished this woman last year.',
    target: 'the woman that Mike thinks the agency punished last year',
    cond: 'cond2',
    ...dp08
};
var dp08_cond3 = {
    setup1: 'Luke wonders which agency punished this woman last year.',
    setup2: 'Mike wonders which agency punished this woman last year.', // stimulus
    response1: 'Luke wonders which agency punished this woman last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Mike wonders which agency punished this woman last year.',
    target: 'the woman that Mike wonders which agency punished her last year',
    cond: 'cond3',
    ...dp08
};

var dp09 = {
    type: 'critical',
    item: 'item09',
    image1: 'img_man1.png',
    image2: 'img_man2.png'
};
var dp09_cond1 = {
    setup1: 'The soldiers questioned this man last week.', // stimulus
    setup2: 'The lawyers questioned this man last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'The lawyers questioned this man last week.',
    stimulus: 'The soldiers questioned this man last week.',
    target: 'the man that the soldiers questioned last week',
    cond: 'cond1',
    ...dp09
};
var dp09_cond2 = {
    setup1: 'Judy thinks the soldiers questioned this man last week.', // stimulus
    setup2: 'Anna thinks the soldiers questioned this man last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Anna thinks the soldiers questioned this man last week.',
    stimulus: 'Judy thinks the soldiers questioned this man last week.',
    target: 'the man that Judy thinks the soldiers questioned last week',
    cond: 'cond2',
    ...dp09
};
var dp09_cond3 = {
    setup1: 'Judy wonders which soldiers questioned this man last week.', // stimulus
    setup2: 'Anna wonders which soldiers questioned this man last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Anna wonders which soldiers questioned this man last week.',
    stimulus: 'Judy wonders which soldiers questioned this man last week.',
    target: 'the man that Judy wonders which soldiers questioned him last week',
    cond: 'cond3',
    ...dp09
};

var dp10 = {
    type: 'critical',
    item: 'item10',
    image1: 'img_woman1.png',
    image2: 'img_woman2.png'
};
var dp10_cond1 = {
    setup1: 'The agency recruited this woman last month.',
    setup2: 'The firm recruited this woman last month.', // stimulus
    response1: 'The agency recruited this woman last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'The firm recruited this woman last month.',
    target: 'the woman that the firm recruited last month',
    cond: 'cond1',
    ...dp10
};
var dp10_cond2 = {
    setup1: 'John thinks the firm recruited this woman last month.',
    setup2: 'Dave thinks the firm recruited this woman last month.', // stimulus
    response1: 'John thinks the firm recruited this woman last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Dave thinks the firm recruited this woman last month.',
    target: 'the woman that Dave thinks the firm recruited last month',
    cond: 'cond2',
    ...dp10
};
var dp10_cond3 = {
    setup1: 'John wonders which firm recruited this woman last month.',
    setup2: 'Dave wonders which firm recruited this woman last month.', // stimulus
    response1: 'John wonders which firm recruited this woman last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Dave wonders which firm recruited this woman last month.',
    target: 'the woman that Dave wonders which firm recruited her last month',
    cond: 'cond3',
    ...dp10
};

var dp11 = {
    type: 'critical',
    item: 'item11',
    image1: 'img_man3.png',
    image2: 'img_man4.png'
};
var dp11_cond1 = {
    setup1: 'The officials released this man last month.', // stimulus
    setup2: 'The guards released this man last month.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'The guards released this man last month.',
    stimulus: 'The officials released this man last month.',
    target: 'the man that the officials released last month',
    cond: 'cond1',
    ...dp11
};
var dp11_cond2 = {
    setup1: 'Mary thinks the officials released this man last month.', // stimulus
    setup2: 'Lisa thinks the officials released this man last month.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Lisa thinks the officials released this man last month.',
    stimulus: 'Mary thinks the officials released this man last month.',
    target: 'the man that Mary thinks the officials released last month',
    cond: 'cond2',
    ...dp11
};
var dp11_cond3 = {
    setup1: 'Mary wonders which officials released this man last month.', // stimulus
    setup2: 'Lisa wonders which officials released this man last month.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Lisa wonders which officials released this man last month.',
    stimulus: 'Mary wonders which officials released this man last month.',
    target: 'the man that Mary wonders which officials released him last month',
    cond: 'cond3',
    ...dp11
};

var dp12 = {
    type: 'critical',
    item: 'item12',
    image1: 'img_woman3.png',
    image2: 'img_woman4.png'
};
var dp12_cond1 = {
    setup1: 'The officers rescued this woman last night.',
    setup2: 'The guards rescued this woman last night.', // stimulus
    response1: 'The officers rescued this woman last night.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'The guards rescued this woman last night.',
    target: 'the woman that the guards rescued last night',
    cond: 'cond1',
    ...dp12
};
var dp12_cond2 = {
    setup1: 'Mike thinks the guards rescued this woman last night.',
    setup2: 'Bill thinks the guards rescued this woman last night.', // stimulus
    response1: 'Mike thinks the guards rescued this woman last night.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Bill thinks the guards rescued this woman last night.',
    target: 'the woman that Bill thinks the guards rescued last night',
    cond: 'cond2',
    ...dp12
};
var dp12_cond3 = {
    setup1: 'Mike wonders which guards rescued this woman last night.',
    setup2: 'Bill wonders which guards rescued this woman last night.', // stimulus
    response1: 'Mike wonders which guards rescued this woman last night.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Bill wonders which guards rescued this woman last night.',
    target: 'the woman that Bill wonders which guards rescued her last night',
    cond: 'cond3',
    ...dp12
};

var dp13 = {
    type: 'critical',
    item: 'item13',
    image1: 'img_man2.png',
    image2: 'img_man1.png'
};
var dp13_cond1 = {
    setup1: 'The doctors treated this man last week.', // stimulus
    setup2: 'The nurses treated this man last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'The nurses treated this man last week.',
    stimulus: 'The doctors treated this man last week.',
    target: 'the man that the doctors treated last week',
    cond: 'cond1',
    ...dp13
};
var dp13_cond2 = {
    setup1: 'Tina thinks the doctors treated this man last week.', // stimulus
    setup2: 'Judy thinks the doctors treated this man last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Judy thinks the doctors treated this man last week.',
    stimulus: 'Tina thinks the doctors treated this man last week.',
    target: 'the man that Tina thinks the doctors treated last week',
    cond: 'cond2',
    ...dp13
};
var dp13_cond3 = {
    setup1: 'Tina wonders which doctors treated this man last week.', // stimulus
    setup2: 'Judy wonders which doctors treated this man last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Judy wonders which doctors treated this man last week.',
    stimulus: 'Tina wonders which doctors treated this man last week.',
    target: 'the man that Tina wonders which doctors treated him last week',
    cond: 'cond3',
    ...dp13
};

var dp14 = {
    type: 'critical',
    item: 'item14',
    image1: 'img_woman2.png',
    image2: 'img_woman1.png'
};
var dp14_cond1 = {
    setup1: 'The organization welcomed this woman last month.',
    setup2: 'The committee welcomed this woman last month.', // stimulus
    response1: 'The organization welcomed this woman last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'The committee welcomed this woman last month.',
    target: 'the woman that the committee welcomed last month',
    cond: 'cond1',
    ...dp14
};
var dp14_cond2 = {
    setup1: 'Dave thinks the committee welcomed this woman last month.',
    setup2: 'Luke thinks the committee welcomed this woman last month.', // stimulus
    response1: 'Dave thinks the committee welcomed this woman last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Luke thinks the committee welcomed this woman last month.',
    target: 'the woman that Luke thinks the committee welcomed last month',
    cond: 'cond2',
    ...dp14
};
var dp14_cond3 = {
    setup1: 'Dave wonders which committee welcomed this woman last month.',
    setup2: 'Luke wonders which committee welcomed this woman last month.', // stimulus
    response1: 'Dave wonders which committee welcomed this woman last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Luke wonders which committee welcomed this woman last month.',
    target: 'the woman that Luke wonders which committee welcomed her last month',
    cond: 'cond3',
    ...dp14
};

var dp15 = {
    type: 'critical',
    item: 'item15',
    image1: 'img_man4.png',
    image2: 'img_man3.png'
};
var dp15_cond1 = {
    setup1: 'The organization interviewed this man last week.', // stimulus
    setup2: 'The committee interviewed this man last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'The committee interviewed this man last week.',
    stimulus: 'The organization interviewed this man last week.',
    target: 'the man that the organization interviewed last week',
    cond: 'cond1',
    ...dp15
};
var dp15_cond2 = {
    setup1: 'Anna thinks the organization interviewed this man last week.', // stimulus
    setup2: 'Mary thinks the organization interviewed this man last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Mary thinks the organization interviewed this man last week.',
    stimulus: 'Anna thinks the organization interviewed this man last week.',
    target: 'the man that Anna thinks the organization interviewed last week',
    cond: 'cond2',
    ...dp15
};
var dp15_cond3 = {
    setup1: 'Anna wonders which organization interviewed this man last week.', // stimulus
    setup2: 'Mary wonders which organization interviewed this man last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Mary wonders which organization interviewed this man last week.',
    stimulus: 'Anna wonders which organization interviewed this man last week.',
    target: 'the man that Anna wonders which organization interviewed him last week',
    cond: 'cond3',
    ...dp15
};

// --------------------------------------------------------
// SU study: critical items for production task
// --------------------------------------------------------

var sp01 = {
    type: 'critical',
    item: 'item01',
    image1: 'img_man1.png',
    image2: 'img_man2.png'
};
var sp01_cond1 = {
    setup1: 'This man borrowed a book last month.', // stimulus
    setup2: 'This man borrowed a bike last month.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man borrowed a bike last month.',
    stimulus: 'This man borrowed a book last month.',
    target: 'the man that borrowed a book last month',
    cond: 'cond1',
    ...sp01
};
var sp01_cond2 = {
    setup1: 'Mary thinks this man borrowed a book last month.', // stimulus
    setup2: 'Lisa thinks this man borrowed a book last month.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Lisa thinks this man borrowed a book last month.',
    stimulus: 'Mary thinks this man borrowed a book last month.',
    target: 'the man that Mary thinks borrowed a book last month',
    cond: 'cond2',
    ...sp01
};
var sp01_cond3 = {
    setup1: 'Mary wonders which book this man borrowed last month.', // stimulus
    setup2: 'Lisa wonders which book this man borrowed last month.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Lisa wonders which book this man borrowed last month.',
    stimulus: 'Mary wonders which book this man borrowed last month.',
    target: 'the man that Mary wonders which book he borrowed last month',
    cond: 'cond3',
    ...sp01
};

var sp02 = {
    type: 'critical',
    item: 'item02',
    image1: 'img_woman1.png',
    image2: 'img_woman2.png'
};
var sp02_cond1 = {
    setup1: 'This woman bought a farm last month.',
    setup2: 'This woman bought a skirt last month.', // stimulus
    response1: 'This woman bought a farm last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman bought a skirt last month.',
    target: 'the woman that bought a skirt last month',
    cond: 'cond1',
    ...sp02
};
var sp02_cond2 = {
    setup1: 'Mike thinks this woman bought a skirt last month.',
    setup2: 'Bill thinks this woman bought a skirt last month.', // stimulus
    response1: 'Mike thinks this woman bought a skirt last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Bill thinks this woman bought a skirt last month.',
    target: 'the woman that Bill thinks bought a skirt last month',
    cond: 'cond2',
    ...sp02
};
var sp02_cond3 = {
    setup1: 'Mike wonders which skirt this woman bought last month.',
    setup2: 'Bill wonders which skirt this woman bought last month.', // stimulus
    response1: 'Mike wonders which skirt this woman bought last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Bill wonders which skirt this woman bought last month.',
    target: 'the woman that Bill wonders which skirt she bought last month',
    cond: 'cond3',
    ...sp02
};

var sp03 = {
    type: 'critical',
    item: 'item03',
    image1: 'img_man3.png',
    image2: 'img_man4.png'
};
var sp03_cond1 = {
    setup1: 'This man built a house last year.', // stimulus
    setup2: 'This man built a fence last year.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man built a fence last year.',
    stimulus: 'This man built a house last year.',
    target: 'the man that built a house last year',
    cond: 'cond1',
    ...sp03
};
var sp03_cond2 = {
    setup1: 'Tina thinks this man built a house last year.', // stimulus
    setup2: 'Judy thinks this man built a house last year.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Judy thinks this man built a house last year.',
    stimulus: 'Tina thinks this man built a house last year.',
    target: 'the man that Tina thinks built a house last year',
    cond: 'cond2',
    ...sp03
};
var sp03_cond3 = {
    setup1: 'Tina wonders which house this man built last year.', // stimulus
    setup2: 'Judy wonders which house this man built last year.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Judy wonders which house this man built last year.',
    stimulus: 'Tina wonders which house this man built last year.',
    target: 'the man that Tina wonders which house he built last year',
    cond: 'cond3',
    ...sp03
};

var sp04 = {
    type: 'critical',
    item: 'item04',
    image1: 'img_woman3.png',
    image2: 'img_woman4.png'
};
var sp04_cond1 = {
    setup1: 'This woman completed a mission last month.',
    setup2: 'This woman completed a project last month.', // stimulus
    response1: 'This woman completed a mission last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman completed a project last month.',
    target: 'the woman that completed a project last month',
    cond: 'cond1',
    ...sp04
};
var sp04_cond2 = {
    setup1: 'Dave thinks this woman completed a project last month.',
    setup2: 'Luke thinks this woman completed a project last month.', // stimulus
    response1: 'Dave thinks this woman completed a project last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Luke thinks this woman completed a project last month.',
    target: 'the woman that Luke thinks completed a project last month',
    cond: 'cond2',
    ...sp04
};
var sp04_cond3 = {
    setup1: 'Dave wonders which project this woman completed last month.',
    setup2: 'Luke wonders which project this woman completed last month.', // stimulus
    response1: 'Dave wonders which project this woman completed last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Luke wonders which project this woman completed last month.',
    target: 'the woman that Luke wonders which project she completed last month',
    cond: 'cond3',
    ...sp04
};

var sp05 = {
    type: 'critical',
    item: 'item05',
    image1: 'img_man2.png',
    image2: 'img_man1.png'
};
var sp05_cond1 = {
    setup1: 'This man delivered a package last week.', // stimulus
    setup2: 'This man delivered a letter last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man delivered a letter last week.',
    stimulus: 'This man delivered a package last week.',
    target: 'the man that delivered a package last week',
    cond: 'cond1',
    ...sp05
};
var sp05_cond2 = {
    setup1: 'Anna thinks this man delivered a package last week.', // stimulus
    setup2: 'Mary thinks this man delivered a package last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Mary thinks this man delivered a package last week.',
    stimulus: 'Anna thinks this man delivered a package last week.',
    target: 'the man that Anna thinks delivered a package last week',
    cond: 'cond2',
    ...sp05
};
var sp05_cond3 = {
    setup1: 'Anna wonders which package this man delivered last week.', // stimulus
    setup2: 'Mary wonders which package this man delivered last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Mary wonders which package this man delivered last week.',
    stimulus: 'Anna wonders which package this man delivered last week.',
    target: 'the man that Anna wonders which package he delivered last week',
    cond: 'cond3',
    ...sp05
};

var sp06 = {
    type: 'critical',
    item: 'item06',
    image1: 'img_woman2.png',
    image2: 'img_woman1.png'
};
var sp06_cond1 = {
    setup1: 'This woman fixed a machine last month.',
    setup2: 'This woman fixed a computer last month.', // stimulus
    response1: 'This woman fixed a machine last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman fixed a computer last month.',
    target: 'the woman that fixed a computer last month',
    cond: 'cond1',
    ...sp06
};
var sp06_cond2 = {
    setup1: 'Bill thinks this woman fixed a computer last month.',
    setup2: 'John thinks this woman fixed a computer last month.', // stimulus
    response1: 'Bill thinks this woman fixed a computer last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'John thinks this woman fixed a computer last month.',
    target: 'the woman that John thinks fixed a computer last month',
    cond: 'cond2',
    ...sp06
};
var sp06_cond3 = {
    setup1: 'Bill wonders which computer this woman fixed last month.',
    setup2: 'John wonders which computer this woman fixed last month.', // stimulus
    response1: 'Bill wonders which computer this woman fixed last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'John wonders which computer this woman fixed last month.',
    target: 'the woman that John wonders which computer she fixed last month',
    cond: 'cond3',
    ...sp06
};

var sp07 = {
    type: 'critical',
    item: 'item07',
    image1: 'img_man4.png',
    image2: 'img_man3.png'
};
var sp07_cond1 = {
    setup1: 'This man found a wallet last night.', // stimulus
    setup2: 'This man found a necklace last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man found a necklace last night.',
    stimulus: 'This man found a wallet last night.',
    target: 'the man that found a wallet last night',
    cond: 'cond1',
    ...sp07
};
var sp07_cond2 = {
    setup1: 'Lisa thinks this man found a wallet last night.', // stimulus
    setup2: 'Tina thinks this man found a wallet last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Tina thinks this man found a wallet last night.',
    stimulus: 'Lisa thinks this man found a wallet last night.',
    target: 'the man that Lisa thinks found a wallet last night',
    cond: 'cond2',
    ...sp07
};
var sp07_cond3 = {
    setup1: 'Lisa wonders which wallet this man found last night.', // stimulus
    setup2: 'Tina wonders which wallet this man found last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Tina wonders which wallet this man found last night.',
    stimulus: 'Lisa wonders which wallet this man found last night.',
    target: 'the man that Lisa wonders which wallet he found last night',
    cond: 'cond3',
    ...sp07
};

var sp08 = {
    type: 'critical',
    item: 'item08',
    image1: 'img_woman4.png',
    image2: 'img_woman3.png'
};
var sp08_cond1 = {
    setup1: 'This woman made a video last year.',
    setup2: 'This woman made a website last year.', // stimulus
    response1: 'This woman made a video last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman made a website last year.',
    target: 'the woman that made a website last year',
    cond: 'cond1',
    ...sp08
};
var sp08_cond2 = {
    setup1: 'Luke thinks this woman made a website last year.',
    setup2: 'Mike thinks this woman made a website last year.', // stimulus
    response1: 'Luke thinks this woman made a website last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Mike thinks this woman made a website last year.',
    target: 'the woman that Mike thinks made a website last year',
    cond: 'cond2',
    ...sp08
};
var sp08_cond3 = {
    setup1: 'Luke wonders which website this woman made last year.',
    setup2: 'Mike wonders which website this woman made last year.', // stimulus
    response1: 'Luke wonders which website this woman made last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Mike wonders which website this woman made last year.',
    target: 'the woman that Mike wonders which website she made last year',
    cond: 'cond3',
    ...sp08
};

var sp09 = {
    type: 'critical',
    item: 'item09',
    image1: 'img_man1.png',
    image2: 'img_man2.png'
};
var sp09_cond1 = {
    setup1: 'This man ordered a pizza last night.', // stimulus
    setup2: 'This man ordered a dish last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man ordered a dish last night.',
    stimulus: 'This man ordered a pizza last night.',
    target: 'the man that ordered a pizza last night',
    cond: 'cond1',
    ...sp09
};
var sp09_cond2 = {
    setup1: 'Judy thinks this man ordered a pizza last night.', // stimulus
    setup2: 'Anna thinks this man ordered a pizza last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Anna thinks this man ordered a pizza last night.',
    stimulus: 'Judy thinks this man ordered a pizza last night.',
    target: 'the man that Judy thinks ordered a pizza last night',
    cond: 'cond2',
    ...sp09
};
var sp09_cond3 = {
    setup1: 'Judy wonders which pizza this man ordered last night.', // stimulus
    setup2: 'Anna wonders which pizza this man ordered last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Anna wonders which pizza this man ordered last night.',
    stimulus: 'Judy wonders which pizza this man ordered last night.',
    target: 'the man that Judy wonders which pizza he ordered last night',
    cond: 'cond3',
    ...sp09
};

var sp10 = {
    type: 'critical',
    item: 'item10',
    image1: 'img_woman1.png',
    image2: 'img_woman2.png'
};
var sp10_cond1 = {
    setup1: 'This woman received a message last week.',
    setup2: 'This woman received an award last week.', // stimulus
    response1: 'This woman received a message last week.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman received an award last week.',
    target: 'the woman that received an award last week',
    cond: 'cond1',
    ...sp10
};
var sp10_cond2 = {
    setup1: 'John thinks this woman received an award last week.',
    setup2: 'Dave thinks this woman received an award last week.', // stimulus
    response1: 'John thinks this woman received an award last week.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Dave thinks this woman received an award last week.',
    target: 'the woman that Dave thinks received an award last week',
    cond: 'cond2',
    ...sp10
};
var sp10_cond3 = {
    setup1: 'John wonders which award this woman received last week.',
    setup2: 'Dave wonders which award this woman received last week.', // stimulus
    response1: 'John wonders which award this woman received last week.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Dave wonders which award this woman received last week.',
    target: 'the woman that Dave wonders which award she received last week',
    cond: 'cond3',
    ...sp10
};

var sp11 = {
    type: 'critical',
    item: 'item11',
    image1: 'img_man3.png',
    image2: 'img_man4.png'
};
var sp11_cond1 = {
    setup1: 'This man rented an apartment last year.', // stimulus
    setup2: 'This man rented a car last year.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man rented a car last year.',
    stimulus: 'This man rented an apartment last year.',
    target: 'the man that rented an apartment last year',
    cond: 'cond1',
    ...sp11
};
var sp11_cond2 = {
    setup1: 'Mary thinks this man rented an apartment last year.', // stimulus
    setup2: 'Lisa thinks this man rented an apartment last year.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Lisa thinks this man rented an apartment last year.',
    stimulus: 'Mary thinks this man rented an apartment last year.',
    target: 'the man that Mary thinks rented an apartment last year',
    cond: 'cond2',
    ...sp11
};
var sp11_cond3 = {
    setup1: 'Mary wonders which apartment this man rented last year.', // stimulus
    setup2: 'Lisa wonders which apartment this man rented last year.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Lisa wonders which apartment this man rented last year.',
    stimulus: 'Mary wonders which apartment this man rented last year.',
    target: 'the man that Mary wonders which apartment he rented last year',
    cond: 'cond3',
    ...sp11
};

var sp12 = {
    type: 'critical',
    item: 'item12',
    image1: 'img_woman3.png',
    image2: 'img_woman4.png'
};
var sp12_cond1 = {
    setup1: 'This woman sold a cafe last year.',
    setup2: 'This woman sold a building last year.', // stimulus
    response1: 'This woman sold a cafe last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman sold a building last year.',
    target: 'the woman that sold a building last year',
    cond: 'cond1',
    ...sp12
};
var sp12_cond2 = {
    setup1: 'Mike thinks this woman sold a building last year.',
    setup2: 'Bill thinks this woman sold a building last year.', // stimulus
    response1: 'Mike thinks this woman sold a building last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Bill thinks this woman sold a building last year.',
    target: 'the woman that Bill thinks sold a building last year',
    cond: 'cond2',
    ...sp12
};
var sp12_cond3 = {
    setup1: 'Mike wonders which building this woman sold last year.',
    setup2: 'Bill wonders which building this woman sold last year.', // stimulus
    response1: 'Mike wonders which building this woman sold last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Bill wonders which building this woman sold last year.',
    target: 'the woman that Bill wonders which building she sold last year',
    cond: 'cond3',
    ...sp12
};

var sp13 = {
    type: 'critical',
    item: 'item13',
    image1: 'img_man2.png',
    image2: 'img_man1.png'
};
var sp13_cond1 = {
    setup1: 'This man sent an email last week.', // stimulus
    setup2: 'This man sent a check last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man sent a check last week.',
    stimulus: 'This man sent an email last week.',
    target: 'the man that sent an email last week',
    cond: 'cond1',
    ...sp13
};
var sp13_cond2 = {
    setup1: 'Tina thinks this man sent an email last week.', // stimulus
    setup2: 'Judy thinks this man sent an email last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Judy thinks this man sent an email last week.',
    stimulus: 'Tina thinks this man sent an email last week.',
    target: 'the man that Tina thinks sent an email last week',
    cond: 'cond2',
    ...sp13
};
var sp13_cond3 = {
    setup1: 'Tina wonders which email this man sent last week.', // stimulus
    setup2: 'Judy wonders which email this man sent last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Judy wonders which email this man sent last week.',
    stimulus: 'Tina wonders which email this man sent last week.',
    target: 'the man that Tina wonders which email he sent last week',
    cond: 'cond3',
    ...sp13
};

var sp14 = {
    type: 'critical',
    item: 'item14',
    image1: 'img_woman2.png',
    image2: 'img_woman1.png'
};
var sp14_cond1 = {
    setup1: 'This woman submitted a proposal last week.',
    setup2: 'This woman submitted an application last week.', // stimulus
    response1: 'This woman submitted a proposal last week.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman submitted an application last week.',
    target: 'the woman that submitted an application last week',
    cond: 'cond1',
    ...sp14
};
var sp14_cond2 = {
    setup1: 'Dave thinks this woman submitted an application last week.',
    setup2: 'Luke thinks this woman submitted an application last week.', // stimulus
    response1: 'Dave thinks this woman submitted an application last week.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Luke thinks this woman submitted an application last week.',
    target: 'the woman that Luke thinks submitted an application last week',
    cond: 'cond2',
    ...sp14
};
var sp14_cond3 = {
    setup1: 'Dave wonders which application this woman submitted last week.',
    setup2: 'Luke wonders which application this woman submitted last week.', // stimulus
    response1: 'Dave wonders which application this woman submitted last week.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'Luke wonders which application this woman submitted last week.',
    target: 'the woman that Luke wonders which application she submitted last week',
    cond: 'cond3',
    ...sp14
};

var sp15 = {
    type: 'critical',
    item: 'item15',
    image1: 'img_man4.png',
    image2: 'img_man3.png'
};
var sp15_cond1 = {
    setup1: 'This man wore a coat last night.', // stimulus
    setup2: 'This man wore a hat last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man wore a hat last night.',
    stimulus: 'This man wore a coat last night.',
    target: 'the man that wore a coat last night',
    cond: 'cond1',
    ...sp15
};
var sp15_cond2 = {
    setup1: 'Anna thinks this man wore a coat last night.', // stimulus
    setup2: 'Mary thinks this man wore a coat last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Mary thinks this man wore a coat last night.',
    stimulus: 'Anna thinks this man wore a coat last night.',
    target: 'the man that Anna thinks wore a coat last night',
    cond: 'cond2',
    ...sp15
};
var sp15_cond3 = {
    setup1: 'Anna wonders which coat this man wore last night.', // stimulus
    setup2: 'Mary wonders which coat this man wore last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'Mary wonders which coat this man wore last night.',
    stimulus: 'Anna wonders which coat this man wore last night.',
    target: 'the man that Anna wonders which coat he wore last night',
    cond: 'cond3',
    ...sp15
};

// --------------------------------------------------------
// fillers for production tasks
// --------------------------------------------------------

var fp01 = {
    type: 'filler',
    item: 'item16',
    set: 'set1',
    image1: 'img_woman1.png',
    image2: 'img_woman2.png',
    setup1: 'This woman wonders which criminals attacked Bill last night.',
    setup2: 'This woman wonders which criminals attacked John last night.', // stimulus
    response1: 'This woman wonders which criminals attacked Bill last night.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman wonders which criminals attacked John last night.',
    target: 'the woman that wonders which criminals attacked John last night'
};

var fp02 = {
    type: 'filler',
    item: 'item17',
    set: 'set1',
    image1: 'img_man1.png',
    image2: 'img_man2.png',
    setup1: 'This man wonders which reporters criticized Mary last month.', // stimulus
    setup2: 'This man wonders which reporters criticized Lisa last month.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man wonders which reporters criticized Lisa last month.',
    stimulus: 'This man wonders which reporters criticized Mary last month.',
    target: 'the man that wonders which reporters criticized Mary last month'
};

var fp03 = {
    type: 'filler',
    item: 'item18',
    set: 'set1',
    image1: 'img_woman3.png',
    image2: 'img_woman4.png',
    setup1: 'This woman wonders which workers threatened Mike last year.',
    setup2: 'This woman wonders which workers threatened Bill last year.', // stimulus
    response1: 'This woman wonders which workers threatened Mike last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman wonders which workers threatened Bill last year.',
    target: 'the woman that wonders which workers threatened Bill last year'
};

var fp04 = {
    type: 'filler',
    item: 'item19',
    set: 'set1',
    image1: 'img_man3.png',
    image2: 'img_man4.png',
    setup1: 'This man wonders which magazine Lisa read last week.', // stimulus
    setup2: 'This man wonders which newspaper Lisa read last week.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man wonders which newspaper Lisa read last week.',
    stimulus: 'This man wonders which magazine Lisa read last week.',
    target: 'the man that wonders which magazine Lisa read last week'
};

var fp05 = {
    type: 'filler',
    item: 'item20',
    set: 'set1',
    image1: 'img_woman2.png',
    image2: 'img_woman1.png',
    setup1: 'This woman wonders which bus Mike drove last week.',
    setup2: 'This woman wonders which truck Mike drove last week.', // stimulus
    response1: 'This woman wonders which bus Mike drove last week.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman wonders which truck Mike drove last week.',
    target: 'the woman that wonders which truck Mike drove last week'
};

var fp06 = {
    type: 'filler',
    item: 'item21',
    set: 'set2',
    image1: 'img_man2.png',
    image2: 'img_man1.png',
    setup1: 'This man says Tina baked a cake last night.', // stimulus
    setup2: 'This man says Tina baked a pie last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man says Tina baked a pie last night.',
    stimulus: 'This man says Tina baked a cake last night.',
    target: 'the man that says Tina baked a cake last night'
};

var fp07 = {
    type: 'filler',
    item: 'item22',
    set: 'set2',
    image1: 'img_woman4.png',
    image2: 'img_woman3.png',
    setup1: 'This woman says Luke watched a play last week.',
    setup2: 'This woman says Luke watched a documentary last week.', // stimulus
    response1: 'This woman says Luke watched a play last week.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman says Luke watched a documentary last week.',
    target: 'the woman that says Luke watched a documentary last week'
};

var fp08 = {
    type: 'filler',
    item: 'item23',
    set: 'set2',
    image1: 'img_man4.png',
    image2: 'img_man3.png',
    setup1: 'This man says Judy opened a shop last month.', // stimulus
    setup2: 'This man says Judy opened a store last month.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man says Judy opened a store last month.',
    stimulus: 'This man says Judy opened a shop last month.',
    target: 'the man that says Judy opened a shop last month'
};

var fp09 = {
    type: 'filler',
    item: 'item24',
    set: 'set2',
    image1: 'img_woman1.png',
    image2: 'img_woman2.png',
    setup1: 'This woman says the activists sued John last year.',
    setup2: 'This woman says the activists sued Dave last year.', // stimulus
    response1: 'This woman says the activists sued John last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman says the activists sued Dave last year.',
    target: 'the woman that says the activists sued Dave last year'
};

var fp10 = {
    type: 'filler',
    item: 'item25',
    set: 'set2',
    image1: 'img_man1.png',
    image2: 'img_man2.png',
    setup1: 'This man says the students teased Lisa last night.', // stimulus
    setup2: 'This man says the students teased Tina last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man says the students teased Tina last night.',
    stimulus: 'This man says the students teased Lisa last night.',
    target: 'the man that says the students teased Lisa last night'
};

var fp11 = {
    type: 'filler',
    item: 'item26',
    set: 'set3',
    image1: 'img_woman3.png',
    image2: 'img_woman4.png',
    setup1: 'This woman says John directed a show last year.',
    setup2: 'This woman says John directed a movie last year.', // stimulus
    response1: 'This woman says John directed a show last year.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman says John directed a movie last year.',
    target: 'the woman that says John directed a movie last year'
};

var fp12 = {
    type: 'filler',
    item: 'item27',
    set: 'set3',
    image1: 'img_man3.png',
    image2: 'img_man4.png',
    setup1: 'This man says Judy told a story last night.', // stimulus
    setup2: 'This man says Judy told a joke last night.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man says Judy told a joke last night.',
    stimulus: 'This man says Judy told a story last night.',
    target: 'the man that says Judy told a story last night'
};

var fp13 = {
    type: 'filler',
    item: 'item28',
    set: 'set3',
    image1: 'img_woman2.png',
    image2: 'img_woman1.png',
    setup1: 'This woman says Mike wrote a novel last month.',
    setup2: 'This woman says Mike wrote a poem last month.', // stimulus
    response1: 'This woman says Mike wrote a novel last month.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman says Mike wrote a poem last month.',
    target: 'the woman that says Mike wrote a poem last month'
};

var fp14 = {
    type: 'filler',
    item: 'item29',
    set: 'set3',
    image1: 'img_man2.png',
    image2: 'img_man1.png',
    setup1: 'This man says the sailors visited Anna last month.', // stimulus
    setup2: 'This man says the sailors visited Mary last month.',
    response1: '<b><u>Which man is this?</b></u>', // question
    response2: 'This man says the sailors visited Mary last month.',
    stimulus: 'This man says the sailors visited Anna last month.',
    target: 'the man that says the sailors visited Anna last month'
};

var fp15 = {
    type: 'filler',
    item: 'item30',
    set: 'set3',
    image1: 'img_woman4.png',
    image2: 'img_woman3.png',
    setup1: 'This woman says the firefighters saved John last week.',
    setup2: 'This woman says the firefighters saved Dave last week.', // stimulus
    response1: 'This woman says the firefighters saved John last week.',
    response2: '<b><u>Which woman is this?</b></u>', // question
    stimulus: 'This woman says the firefighters saved Dave last week.',
    target: 'the woman that says the firefighters saved Dave last week'
};

// --------------------------------------------------------
// running lists for production tasks
// --------------------------------------------------------

// fillers

var pfill = [
    fp01, fp02, fp03, fp04, fp05,
    fp06, fp07, fp08, fp09, fp10,
    fp11, fp12, fp13, fp14, fp15
];

// DO study

var dpx1 = [
    dp01_cond1, dp02_cond2, dp03_cond3,
    dp04_cond1, dp05_cond2, dp06_cond3,
    dp07_cond1, dp08_cond2, dp09_cond3,
    dp10_cond1, dp11_cond2, dp12_cond3,
    dp13_cond1, dp14_cond2, dp15_cond3
];

var dpx2 = [
    dp01_cond2, dp02_cond3, dp03_cond1,
    dp04_cond2, dp05_cond3, dp06_cond1,
    dp07_cond2, dp08_cond3, dp09_cond1,
    dp10_cond2, dp11_cond3, dp12_cond1,
    dp13_cond2, dp14_cond3, dp15_cond1
];

var dpx3 = [
    dp01_cond3, dp02_cond1, dp03_cond2,
    dp04_cond3, dp05_cond1, dp06_cond2,
    dp07_cond3, dp08_cond1, dp09_cond2,
    dp10_cond3, dp11_cond1, dp12_cond2,
    dp13_cond3, dp14_cond1, dp15_cond2
];

var dp_test = [dp01_cond1, fp01, dp02_cond3]; // short list for testing

// SU study

var spx1 = [
    sp01_cond1, sp02_cond2, sp03_cond3,
    sp04_cond1, sp05_cond2, sp06_cond3,
    sp07_cond1, sp08_cond2, sp09_cond3,
    sp10_cond1, sp11_cond2, sp12_cond3,
    sp13_cond1, sp14_cond2, sp15_cond3
];

var spx2 = [
    sp01_cond2, sp02_cond3, sp03_cond1,
    sp04_cond2, sp05_cond3, sp06_cond1,
    sp07_cond2, sp08_cond3, sp09_cond1,
    sp10_cond2, sp11_cond3, sp12_cond1,
    sp13_cond2, sp14_cond3, sp15_cond1
];

var spx3 = [
    sp01_cond3, sp02_cond1, sp03_cond2,
    sp04_cond3, sp05_cond1, sp06_cond2,
    sp07_cond3, sp08_cond1, sp09_cond2,
    sp10_cond3, sp11_cond1, sp12_cond2,
    sp13_cond3, sp14_cond1, sp15_cond2
];

var sp_test = [sp01_cond1, fp01, sp02_cond3]; // short list for testing

// --------------------------------------------------------
// production practice
// --------------------------------------------------------

var inactive_btn = '.inactive-btn {display: inline-block; padding: 6px 12px; margin: 0px; font-size: 14px; font-weight: 400; font-family: "Open Sans", "Arial", sans-serif; cursor: pointer; line-height: 1.4; text-align: center; white-space: nowrap; vertical-align: middle; background-image: none; border: 1px solid transparent; border-radius: 4px; color: #333; background-color: #fff; border-color: #ccc; pointer-events: none;}';

var ept_prac1 = {
    item: 'ept_prac1',
    stimulus1: '<style> table {margin-left:auto; margin-right:auto;} td {text-align:left;}'+inactive_btn+'</style>'+
    '<div style="margin:0;"><table>'+
    '<tr><td><img src="img_woman1.png" style="width:100px;"></img></td>'+
    '<td>This woman wrote a book.</td></tr>'+
    '<tr><td><img src="img_woman2.png" style="width:100px;"></img></td>'+
    '<td>This woman read a book.</td></tr></table></div>',
    stimulus2: '<style> table {margin-left:auto; margin-right:auto;} td {text-align:left;}'+inactive_btn+' div {margin:0; padding:0; border:0; outline:0;}</style>'+
    '<div style="margin:0;"><table>'+
    '<tr><td><img src="img_woman1.png" style="width:100px;"></img></td>'+
    '<td>This woman wrote a book.</td></tr>'+
    '<tr><td><img src="img_woman2.png" style="width:100px;"></img></td>'+
    '<td><b><u>Which woman is this?</b></u></td></tr></table></div>',
    choices: ['the one that wrote a book', 'This woman read a book', 'the one that read a book', 'This woman wrote a book'],
    correct: '2',
};

var ept_prac2 = {
    item: 'ept_prac2',
    stimulus1: '<style> table {margin-left:auto; margin-right:auto;} td {text-align:left;}'+inactive_btn+' .center {display: flex; justify-content: center; align-items: center;} </style>'+
    '<table>'+
    '<tr><td><img src="img_man2.png" style="width:100px;"></img></td>'+
    '<td>This man walked to the park.</td></tr>'+
    '<tr><td><img src="img_man1.png" style="width:100px;"></img></td>'+
    '<td>This man walked to the mall.</td></tr></table>',
    stimulus2: '<style> table {margin-left:auto; margin-right:auto;} td {text-align:left;}'+inactive_btn+' .center {display: flex; justify-content: center; align-items: center;} </style>'+
    '<table>'+
    '<tr><td><img src="img_man2.png" style="width:100px;"></img></td>'+
    '<td><b><u>Which man is this?</b></u></td></tr>'+
    '<tr><td><img src="img_man1.png" style="width:100px;"></img></td>'+
    '<td>This man walked to the mall.</td></tr></table>',
    choices: ['This man walked to the mall', 'the man that walked to the park', 'This man walked to the park', 'the man that walked to the mall'],
    correct: '1',
};

var ept_prac3 = {
    item: 'ept_prac3',
    stimulus1: '<style> table {margin-left:auto; margin-right:auto;} td {text-align:left;}'+inactive_btn+'</style>'+
    '<div style="margin:0;"><table>'+
    '<tr><td><img src="img_woman4.png" style="width:100px;"></img></td>'+
    '<td>This woman likes John.</td></tr>'+
    '<tr><td><img src="img_woman3.png" style="width:100px;"></img></td>'+
    '<td>This woman likes Bill.</td></tr></table></div>',
    stimulus2: '<style> table {margin-left:auto; margin-right:auto;} td {text-align:left;}'+inactive_btn+' div {margin:0; padding:0; border:0; outline:0;}</style>'+
    '<div style="margin:0;"><table>'+
    '<tr><td><img src="img_woman4.png" style="width:100px;"></img></td>'+
    '<td>This woman likes John.</td></tr>'+
    '<tr><td><img src="img_woman3.png" style="width:100px;"></img></td>'+
    '<td><b><u>Which woman is this?</b></u></td></tr></table></div>',
    choices: ['This woman likes Bill', 'the woman that likes John', 'This woman likes John', 'the woman that likes Bill'],
    correct: '3',
};

var ept_prac_list = [ept_prac1, ept_prac2, ept_prac3];