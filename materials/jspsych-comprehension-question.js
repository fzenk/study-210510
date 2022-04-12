/**
 * jspsych-comprehension-question
 * written by Fred Zenker using Shane Martin's 
 * 'jspsych-survey-multi-choice' plugin as a model
 */

jsPsych.plugins['comprehension-question'] = (function() {
  var plugin = {};

  plugin.info = {
    name: 'comprehension-question',
    parameters: {
      prompt: {
          type: jsPsych.plugins.parameterType.STRING,
          pretty_name: 'Prompt',
          default: undefined,
          description: 'Prompt text'
      },
      text_left: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Left Side Text',
        default: '',
        description: 'Text to display to the left of the input fields'
      },
      text_center: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Center Text',
        default: '',
        description: 'Text to display between the two input fields'
      },
      text_right: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Right Side Text',
        default: '',
        description: 'Text to display to the right of the input fields'
      },
      options_left: {
          type: jsPsych.plugins.parameterType.STRING,
          pretty_name: 'Left Side Options',
          array: true,
          default: undefined,
          description: 'Response options for the first input field'
      },
      options_right: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Right Side Options',
        array: true,
        default: undefined,
        description: 'Response options for the second input field'
      },
      correct_left: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Left Side Correct Answer',
        default: null,
        description: 'Correct answer for the first input field'
      },
      correct_right: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Right Side Correct Answer',
        default: null,
        description: 'Correct answer for second input field'
      },
      button_label: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Button label',
        default:  'Continue',
        description: 'Label for the button to continue to the next trial'
      },
    }
  }
  plugin.trial = function(display_element, trial) {
    var plugin_id_name = "jspsych-comprehension-question";

    var html = "";

    // inject CSS for trial
    html += '<style id="jspsych-comprehension-question-css">';
    html += 
      ".jspsych-comprehension-question-prompt { margin-top: 2em; margin-bottom: 2em; text-align: left; }"+
      "table {border-collapse:collapse; margin-bottom: 2em;}"+
      "td {padding: 0px 5px 0px 5px;}"+
      ".jspsych-comprehension-question-option { line-height: 2; }"+
      "label.jspsych-comprehension-question-text input[type='radio'] {margin-right: .8em;}";
    html += '</style>';

    // show prompt
    if(trial.prompt !== null){
      html += '<div id="jspsych-comprehension-question-prompt" class="jspsych-comprehension-question-prompt" style="text-align: center;">'+trial.prompt+'</div>';
    }

    // form id
    html += '<form id="jspsych-comprehension-question" autocomplete="off">';

    // create table
    html += '<table><tr>';

    // add text on left side of input fields
    html += '<td style="text-align: center;">'+trial.text_left+'</td>';

    // add blank cell to left of trial.word
    html += '<td style="border: 1.5px solid black;"></td>';

    // add word or words to display between two input menus
    html += '<td style="text-align: center;">'+trial.text_center+'</td>';

    // add blank cell to right of trial.word
    html += '<td style="border: 1.5px solid black;"></td>';

    // add text on right side of input fields
    html += '<td style="text-align: center;"> '+trial.text_right+' </td></tr>';
    
    // create first input menu
    html += '<tr><td></td><td style="text-align: left; background-color:#f7f3f2;  box-shadow:2px 2px 5px #999;">'+
    '<div id="options_left_container">';
    for (var j = 0; j < trial.options_left.length; j++) {
      // add labels
      var options_left_option_id_name = "jspsych-comprehension-question-option-options_left-"+j;
      var options_left_input_name = 'jspsych-comprehension-question-response-options_left';
      var options_left_input_id = 'jspsych-comprehension-question-response-options_left-'+j;
      // add radio button container
      html += '<div id="'+options_left_option_id_name+'" class="jspsych-comprehension-question-option">';
      html += '<label class="jspsych-comprehension-question-text" for="'+options_left_input_id+'">';
      html += '<input type="radio" name="'+options_left_input_name+'" id="'+options_left_input_id+'" value="'+trial.options_left[j]+'" required></input>';
      html += trial.options_left[j]+'</label>';
      html += '</div>';
    };
    html += '</div></td>';

    // add blank cell between the two menus
    html += '<td></td>';

    // create second radio input menu
    html += 
    '<td style="text-align: left; background-color:#f7f3f2; box-shadow:2px 2px 5px #999;">'+
    '<div id="options_right_container">';
    for (var j = 0; j < trial.options_right.length; j++) {
      // add labels
      var options_right_option_id_name = "jspsych-comprehension-question-option-options_right-"+j;
      var options_right_input_name = 'jspsych-comprehension-question-response-options_right';
      var options_right_input_id = 'jspsych-comprehension-question-response-options_right-'+j;
      // add radio button container
      html += '<div id="'+options_right_option_id_name+'" class="jspsych-comprehension-question-option">';
      html += '<label class="jspsych-comprehension-question-text" for="'+options_right_input_id+'">';
      html += '<input type="radio" name="'+options_right_input_name+'" id="'+options_right_input_id+'" value="'+trial.options_right[j]+'" required></input>';
      html += trial.options_right[j]+'</label>';
      html += '</div>';
    };
    html += '</div>';

    // close table
    html += '</td><td></td></tr></table>';
    
    // add submit button
    html += '<input type="submit" id="'+plugin_id_name+'-next" class="'+plugin_id_name+' jspsych-btn"' + (trial.button_label ? ' value="'+trial.button_label + '"': '') + '></input>';
    html += '</form>';

    // render
    display_element.innerHTML = html;

    document.querySelector('form').addEventListener('submit', function(event) {
      event.preventDefault();
      // measure response time
      var endTime = performance.now();
      var response_time = endTime - startTime;

    // create object to hold responses
    var container1 = display_element.querySelector('#options_left_container');
    if(container1.querySelector("input[type=radio]:checked") !== null){
      val1 = container1.querySelector("input[type=radio]:checked").value;
    };
    var container2 = display_element.querySelector('#options_right_container');
    var val2 = "";
    if(container2.querySelector("input[type=radio]:checked") !== null){
      val2 = container2.querySelector("input[type=radio]:checked").value;
    };

    // check accuracy
    if (val1 == trial.correct_left & val2 == trial.correct_right) {
      acc = true
    } else {
      acc = false
    };

      // save data
      var trial_data = {
        rt: response_time,
        response1: val1,
        response2: val2,
        accuracy: acc
      };
      display_element.innerHTML = '';

      // next trial
      jsPsych.finishTrial(trial_data);
    });

    var startTime = performance.now();
  };

  return plugin;
})();
