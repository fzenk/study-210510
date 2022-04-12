/**
 * jspsych-elicited-production-response
 * 
 * adapted by Fred Zenker from code written by...
 * 
 * Matt Jaquiery, Feb 2018 (https://github.com/mjaquiery)
 * Becky Gilbert, Apr 2020 (https://github.com/becky-gilbert)
 *
 * plugin for displaying a stimulus and getting an audio response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["elicited-production-response"] = (function() {

    let plugin = {};

    plugin.info = {
        name: 'elicited-production-response',
        description: 'Present an image and retrieve an audio response',
        parameters: {
            stimulus1: {
                type: jsPsych.plugins.parameterType.IMAGE,
                pretty_name: 'Stimulus 1',
                default: undefined,
                description: 'The first image to be displayed'
            },
            stimulus2: {
                type: jsPsych.plugins.parameterType.IMAGE,
                pretty_name: 'Stimulus 2',
                default: undefined,
                description: 'The second image to be displayed'
            },
            buffer_length: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Buffer length',
                default: 6000,
                description: 'Length of the audio buffer.'
            },
            postprocessing: {
                type: jsPsych.plugins.parameterType.FUNCTION,
                pretty_name: 'Postprocessing function',
                default: function(data) {
                    return new Promise(function(resolve) {
                        const blob = new Blob(data, { type: 'audio/webm' });
                        // create URL, which is used to replay the audio file (if allow_playback is true)
                        let url = URL.createObjectURL(blob);
                        var reader = new window.FileReader();
                        reader.readAsDataURL(blob);
                        const readerPromise = new Promise(function(resolveReader) {
                            reader.onloadend = function() {
                                // Create base64 string, which is used to save the audio data in JSON/CSV format.
                                // This has to go inside of a Promise so that the base64 data is converted before the 
                                // higher-level data processing Promise is resolved (since that will pass the base64
                                // data to the onRecordingFinish function).
                                var base64 = reader.result;
                                base64 = base64.split(',')[1];
                                resolveReader(base64);
                            };
                        });
                        readerPromise.then(function(base64) {
                            // After the base64 string has been created we can resolve the higher-level Promise, 
                            // which pass both the base64 data and the URL to the onRecordingFinish function.
                            var processed_data = {url: url, str: base64};
                            resolve(processed_data);
                        });
                    });
                },
                description: 'Function to execute on the audio data prior to saving. '+
                    'This function takes the audio data as an argument, '+
                    'and returns an object with keys called "str" and "url". '+
                    'The str and url values are saved in the trial data as "audio_data" and "audio_url". '+
                    'The url value is used as the audio source to replay the recording if allow_playback is true. '+
                    'By default, the str value is a base64 string which can be saved in the JSON/CSV data and '+
                    'later converted back into an audio file. '+
                    'This parameter can be used to pass a custom function that saves the file using a different '+
                    'method/format and generates an ID that relates this file to the trial data. '+
                    'The custom postprocessing function must return an object with "str" and "url" keys. '+
                    'The url value must be a valid audio source, which is used if allow_playback is true. '+
                    'The str value can be null.'
            },
            allow_playback: {
                type: jsPsych.plugins.parameterType.BOOL,
                pretty_name: 'Allow playback',
                default: true,
                description: 'Whether to allow the participant to play back their '+
                'recording and re-record if unhappy.'
            },
            recording_light: {
                type: jsPsych.plugins.parameterType.HTML_STRING,
                pretty_name: 'Recording light',
                default: '<div id="jspsych-elicited-production-response-light" style="font-weight:bold; color:red;">Recording...</div>',
                description: 'HTML to display while recording is in progress.'
            },
            recording_light_off: {
                type: jsPsych.plugins.parameterType.HTML_STRING,
                pretty_name: 'Recording light (off state)',
                default: '<div id="jspsych-elicited-production-response-light"></div>',
                description: 'HTML to display while recording is not in progress.'
            },
            text1: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Text 1',
                default: null,
                description: 'Any content here will be displayed next to the first image.'
            },
            text2: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Text 2',
                default: null,
                description: 'Any content here will be displayed next to the second image.'
            },
            prompt: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Prompt',
                default: '<i>Press the play button to hear your response</i>',
                description: 'Any content here will be displayed under the button.'
            },
            button1: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Button1',
                default: 'Accept Recording',
                description: 'Label for button to accept recording.'
            },
            button2: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Button2',
                default: 'Record Again',
                description: 'Label for button to record response again.'
            },
            stimulus_duration: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Stimulus duration',
                default: null,
                description: 'How long to show the stimulus.'
            },
			stimulus_height: {
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Image height',
				default: null,
				description: 'Set the image height in pixels'
			},
			stimulus_width: {
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Image width',
				default: 100,
				description: 'Set the image width in pixels'
			},
			maintain_aspect_ratio: {
				type: jsPsych.plugins.parameterType.BOOL,
				pretty_name: 'Maintain aspect ratio',
				default: true,
				description: 'Maintain the aspect ratio after setting width or height'
			},
            margin_vertical: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Margin vertical',
                default: '0px',
                description: 'The vertical margin of the button.'
            },
            margin_horizontal: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Margin horizontal',
                default: '8px',
                description: 'The horizontal margin of the button.'
            },
            response_ends_trial: {
                type: jsPsych.plugins.parameterType.BOOL,
                pretty_name: 'Response ends trial',
                default: false,
                description: 'If true, then trial will end when user responds.'
            },
            wait_for_mic_approval: {
                type: jsPsych.plugins.parameterType.BOOL,
                pretty_name: 'Wait for mic approval',
                default: true,
                description: 'If true, the trial will not start until the participant approves the browser mic request.'
            }
        }
    };

    plugin.trial = function(display_element, trial) {

        if(typeof trial.stimulus1 === 'undefined'){
            console.error('Required parameter "stimulus1" missing in elicited-production-response');
        }

        if(typeof trial.stimulus2 === 'undefined'){
            console.error('Required parameter "stimulus2" missing in elicited-production-response');
        }

        let playbackElements = [];
        // store response
        let response = {
            rt: null,
            audio_data: null
        };
        let recorder = null;
        let start_time = null;

        // add stimulus1
		var html = '<table><tr><td><img src="'+trial.stimulus1+'" id="jspsych-elicited-production-response-stimulus1" style="';
		if(trial.stimulus_height !== null){
		  html += 'height:'+trial.stimulus_height+'px; '
		  if(trial.stimulus_width == null && trial.maintain_aspect_ratio){
			html += 'width: auto; ';
		  }
		}
		if(trial.stimulus_width !== null){
		  html += 'width:'+trial.stimulus_width+'px; '
		  if(trial.stimulus_height == null && trial.maintain_aspect_ratio){
			html += 'height: auto; ';
		  }
		}
        html +='"></img></td><td style="text-align:left">';

        // add text1 if there is one
        if (trial.text1 !== null) {
            html += trial.text1;
        }

        //add stimulus2
		html += '</td></tr><tr><td><img src="'+trial.stimulus2+'" id="jspsych-elicited-production-response-stimulus2" style="';
		if(trial.stimulus_height !== null){
		  html += 'height:'+trial.stimulus_height+'px; '
		  if(trial.stimulus_width == null && trial.maintain_aspect_ratio){
			html += 'width: auto; ';
		  }
		}
		if(trial.stimulus_width !== null){
		  html += 'width:'+trial.stimulus_width+'px; '
		  if(trial.stimulus_height == null && trial.maintain_aspect_ratio){
			html += 'height: auto; ';
		  }
		}
        html +='"></img></td><td style="text-align:left">';

        // add text2 if there is one
        if (trial.text2 !== null) {
            html += trial.text2;
        }

        html += '</td></tr></table>';

        // add prompt if there is one
        //if (trial.prompt !== null) {
            html += '<div id="jspsych-elicited-production-response-prompt" style="visibility:hidden;">'+trial.prompt+'</div>';
        //}

        // add recording off light
        html += '<div id="jspsych-elicited-production-response-recording-container">'+trial.recording_light_off+'</div>';

        // add audio element container with hidden audio element
        html += '<div id="jspsych-elicited-production-response-audio-container"><audio id="jspsych-elicited-production-response-audio" controls style="visibility:hidden;"></audio></div>';

        // add button element with hidden buttons
        html += '<div id="jspsych-elicited-production-response-buttons"><button id="jspsych-elicited-production-response-okay" class="jspsych-audio-response-button jspsych-btn" style="display: inline-block; margin:'+trial.margin_vertical+' '+trial.margin_horizontal+'; visibility:hidden;">'+trial.button1+'</button><button id="jspsych-elicited-production-response-rerecord" class="jspsych-audio-response-button jspsych-btn" style="display: inline-block; margin:'+trial.margin_vertical+' '+trial.margin_horizontal+'; visibility:hidden;">'+trial.button2+'</button></div>';

        function start_trial() {
            display_element.innerHTML = html;
            document.querySelector('#jspsych-elicited-production-response-okay').addEventListener('click', end_trial);
            document.querySelector('#jspsych-elicited-production-response-rerecord').addEventListener('click', start_recording);
            // Add visual indicators to let people know we're recording
            document.querySelector('#jspsych-elicited-production-response-recording-container').innerHTML = trial.recording_light;
            // trial start time
            start_time = performance.now();
            // set timer to hide image if stimulus duration is set
            if (trial.stimulus_duration !== null) {
                jsPsych.pluginAPI.setTimeout(function() {
                    display_element.querySelector('#jspsych-elicited-production-response-stimulus1').style.visibility = 'hidden';
                    display_element.querySelector('#jspsych-elicited-production-response-stimulus2').style.visibility = 'hidden';
                }, trial.stimulus_duration);
            }
            if (!trial.wait_for_mic_approval) {
                start_recording();
            }
        }

        // audio element processing
        function start_recording() {
            // hide existing playback elements
            playbackElements.forEach(function (id) {
                let element = document.getElementById(id);
                element.style.visibility = 'hidden';
            });
            navigator.mediaDevices.getUserMedia({ audio: true, video: false }).then(process_audio);
            if (!trial.wait_for_mic_approval) {
                // Add visual indicators to let people know we're recording
                document.querySelector('#jspsych-elicited-production-response-recording-container').innerHTML = trial.recording_light;
            }
        }
        
        // function to handle responses by the subject
        function process_audio(stream) {

            if (trial.wait_for_mic_approval) {
                if (start_time === null) {
                    start_trial();
                } else {
                    document.querySelector('#jspsych-elicited-production-response-recording-container').innerHTML = trial.recording_light;
                }
            } 

            // This code largely thanks to skyllo at
            // http://air.ghost.io/recording-to-an-audio-file-using-html5-and-js/

            // store streaming data chunks in array
            const chunks = [];
            // create media recorder instance to initialize recording
            // Note: the MediaRecorder function is not supported in Safari or Edge
            recorder = new MediaRecorder(stream);
            recorder.data = [];
            recorder.wrapUp = false;
            recorder.ondataavailable = function(e) {
                // add stream data to chunks
                chunks.push(e.data);
                if (recorder.wrapUp) {
                    if (typeof trial.postprocessing !== 'undefined') {
                        trial.postprocessing(chunks)
                            .then(function(processedData) {
                                onRecordingFinish(processedData);
                            });
                    } else {
                        // should never fire - trial.postprocessing should use the default function if
                        // not passed in via trial parameters
                        onRecordingFinish(chunks);
                    }
                }
            };

            // start recording with 1 second time between receiving 'ondataavailable' events
            recorder.start(1000);
            // setTimeout to stop recording after 4 seconds
            setTimeout(function() {
                // this will trigger one final 'ondataavailable' event and set recorder state to 'inactive'
                recorder.stop();
                recorder.wrapUp = true;
            }, trial.buffer_length);
        }

        function showPlaybackTools(data) {
            // Audio Player
            let playerDiv = display_element.querySelector('#jspsych-elicited-production-response-audio-container');
            let url;
            if (data instanceof Blob) {
                const blob = new Blob(data, { type: 'audio/webm' });
                url = (URL.createObjectURL(blob));
            } else {
                url = data;
            }
            let player = playerDiv.querySelector('#jspsych-elicited-production-response-audio');
            player.src = url;
            player.style.visibility = "visible";
            // Okay/rerecord buttons
            let buttonDiv = document.querySelector('#jspsych-elicited-production-response-buttons');
            let okay = buttonDiv.querySelector('#jspsych-elicited-production-response-okay');
            let rerecord = buttonDiv.querySelector('#jspsych-elicited-production-response-rerecord');
            let prompt_text = document.querySelector('#jspsych-elicited-production-response-prompt');
            okay.style.visibility = 'visible';
            rerecord.style.visibility = 'visible';
            prompt_text.style.visibility = 'visible';            
            // Save ids of things we want to hide later:
            playbackElements = [player.id, okay.id, rerecord.id, prompt_text.id];
        }

        function onRecordingFinish(data) {
            // switch to the off visual indicator
            let light = document.querySelector('#jspsych-elicited-production-response-recording-container');
            if (light !== null)
                light.innerHTML = trial.recording_light_off;
            // measure rt
            let end_time = performance.now();
            let rt = end_time - start_time;
            response.audio_data = data.str;
            response.audio_url = data.url;
            response.rt = rt;

            if (trial.response_ends_trial) {
                end_trial();
            } else if (trial.allow_playback) {  // only allow playback if response doesn't end trial
                showPlaybackTools(response.audio_url);
            } else { 
                // fallback in case response_ends_trial and allow_playback are both false, 
                // which would mean the trial never ends
                end_trial();
            }
        }

        // function to end trial when it is time
        function end_trial() {
            // kill any remaining setTimeout handlers
            jsPsych.pluginAPI.clearAllTimeouts();

            // gather the data to store for the trial
            let trial_data = {
                "rt": response.rt,
                "stimulus1": trial.stimulus1,
                "stimulus2": trial.stimulus2,
                "text1": trial.text1,
                "text2": trial.text2,
                "audio_data": response.audio_data
            };

            // clear the display
            display_element.innerHTML = '';

            // move on to the next trial
            jsPsych.finishTrial(trial_data);
        }

        if (trial.wait_for_mic_approval) {
            start_recording();
        } else {
            start_trial();
        }

    };

    return plugin;
})();
