/*
 * Copyright 2012 The Board of Regents of the University of Wisconsin System.
 * Contributors: Jason Shao, James McCurdy, Enhai Xie, Adam G.W. Halstead,
 * Michael H. Whitney, Nathan DiPiazza, Trey K. Sato and Yury V. Bukhman
 *
 * This file is part of GCAT.
 *
 * GCAT is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GCAT is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GCAT.  If not, see <http://www.gnu.org/licenses/>.
 */


$(document).ready(function(){
	// Three blocks below for hiding the optional user input fields.
	$(".user_enter1").on("change", function(){
  	$(".optional_input1").toggle($(this).hasClass("user_enter1_yes"));
	});

	$(".user_enter2").on("change", function(){
  	$(".optional_input2").toggle($(this).hasClass("user_enter2_yes"));
	});

	$(".user_enter3").on("change", function(){
  	$(".optional_input3").toggle($(this).hasClass("user_enter3_yes"));
	});

	// handle error redirect & browser back cases//////////////////////////////////////////
	/****************************************************
	 * This function checks the radio box for the
	 * corresponding optional div with input field.
	 * If checked show else hide
	 * @return void
	****************************************************/
	function reset_optional_fields(radio, optional_box){
		if(radio && optional_box){
			var radio_ckd = radio.prop('checked');
			if(radio_ckd){
				optional_box.show();
			}
			else {
				optional_box.hide();
			}
		} else{
			console.error("Error: unknown selector in reset_optional_fields()");
		}
	}
	reset_optional_fields($('#assay_plate_type_multiple'), $("#time"));
	reset_optional_fields($("input#assay_blank_value_user"), $("div.optional_input3"));
	reset_optional_fields($("input#assay_transformation_user"), $("div.optional_input2"));
	reset_optional_fields($("input#assay_model_loess"), $("div.optional_input1"));
	//////////////////////////////////////////////////////////////////////////////////////

	$('#new_assay').submit(function() {
			$.loader();
	});

	$("input[name$='assay[plate_type]']").click(function(){
	  var value = $(this).val();
	  if(value=='single') {
	    $("#time").hide();
      $("#assay_blank_value_average_div").show();
	  }
	  else if(value=='multiple') {
	    $("#time").show();
      $("#assay_blank_value_average_div").hide();
	   }
	});

  //automatically change inoculation point to 2 if user selects "take the first OD as blank option" and current inoculation point is 1
  $("#assay_blank_value_default").click(function(){
    if($(this).is(':checked')){
      var start_index_field = $("#assay_start_index");
      if(start_index_field.val() == "1"){
        start_index_field.val("2");
      }
    }
  });

});
