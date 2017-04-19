require "rails_helper"

feature "User fills plate type" do
    scenario "single plate is default" do
        visit new_assay_path
        expect(find('#assay_plate_type_single')).to be_checked
    end

    scenario "timestamp only shows up when multiple is selected",js:true do
	    visit new_assay_path
	    expect(page).not_to have_content('Timestamp format')
	    choose('assay_plate_type_multiple')
	    expect(page).to have_content('Timestamp format')
      choose('assay_plate_type_single')
      expect(page).not_to have_content('Timestamp format')
    end
    
    scenario "Average OD only shows up when single is selected",js:true do
	    visit new_assay_path
	    expect(page).to have_selector('#assay_blank_value_average')
	    choose('assay_plate_type_multiple')
	    expect(page).not_to have_selector('#assay_blank_value_average')
      choose('assay_plate_type_single')
	    expect(page).to have_selector('#assay_blank_value_average')
    end
end
