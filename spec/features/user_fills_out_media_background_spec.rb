require 'rails_helper'

feature 'User fills out media background' do
  scenario 'only show textfield when user_input is selected',js:true do
    visit new_assay_path
    choose('assay_blank_value_user')
    #print page.html
    expect(page).to have_selector('#assay_blank_value_input')
    
    choose('assay_blank_value_default')
    expect(page).not_to have_selector '#assay_blank_value_input'
    
    choose('assay_blank_value_zero')
    expect(page).not_to have_selector '#assay_blank_value_input'
    
    choose('assay_blank_value_average')
    expect(page).not_to have_selector '#assay_blank_value_input'
    
    choose('assay_blank_value_user')
    expect(page).to have_selector '#assay_blank_value_input'
  end
end

