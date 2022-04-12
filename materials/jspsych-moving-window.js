/**
 * jspsych-moving-window
 * written by Fred Zenker working from Josh de Leeuw's
 * YouTube tutorial (https://www.youtube.com/watch?v=XQcsFwAmbiw)
 */

jsPsych.plugins["moving-window"] = (function() {

  var plugin = {};

  plugin.info = {
    name: "moving-window",
    parameters: {
      sent: {
        type: jsPsych.plugins.parameterType.STRING, // BOOL, STRING, INT, FLOAT, FUNCTION, KEYCODE, SELECT, HTML_STRING, IMAGE, AUDIO, VIDEO, OBJECT, COMPLEX
        default: undefined
      },
      key: {
        type: jsPsych.plugins.parameterType.KEYCODE,
        default: ' '
      },
      font_size: {
        type: jsPsych.plugins.parameterType.INT,
        default: 20
      }
    }
  }

  plugin.trial = function(display_element, trial) {

    // initalize some variables
    var words = trial.sent;
    var trial_data = { words }; // data object for the trial
    var n_words = words.split(' ').length; // number of words in the trial
    var rt = []; // empty array for collecting RTs
    var current_word = null; // current word

    // create a function for generating the stimulus with moving window
    function create_moving_window(words, position){
      var word_list = words.split(' ');
      var stimulus = word_list.map(function(word, index){
        if(index==position){
          return word;
        } else {
          return "-".repeat(word.length);
        }
      }).join(' ')
      return stimulus;
    }

    // create a function for showing the stimulus and collecting the response
    function show_stimulus(position){
      display_element.innerHTML = '<p style="font-size: '+trial.font_size+'px; font-family:\'Source Code Pro\', monospace;">' + create_moving_window(words, position) + '</p>';

      jsPsych.pluginAPI.getKeyboardResponse({
        callback_function: after_response,
        valid_responses: [trial.key],
        rt_method: 'performance',
        persist: false,
        allow_held_key: false
      });
    }

    // create a function for handling a response
    function after_response(response_info){
      if(current_word != null){
        rt.push(response_info.rt);
      }
      if(current_word == null){
        current_word = 0
      } else {
        current_word++;
      }
      if(current_word == n_words){
        end_trial();
      } else {
        show_stimulus(current_word);
      }
    }
    
    // create a function to handle ending the trial
    function end_trial(){
      trial_data.rt = JSON.stringify(rt);

      display_element.innerHTML = "";

      jsPsych.finishTrial(trial_data)
    }

    // show the first stimulus
    show_stimulus(current_word);
    
  };

  return plugin;
})();
