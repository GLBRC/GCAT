require 'rails_helper'

feature 'User fills out points to ignore' do
  scenario 'user enters letters' do
    visit new_assay_path
    fill_in('assay_remove_points', :with=>'a')
    click_button 'Create Assay'
    expect(page).to have_text 'Remove points - Please Enter a comma-separated list of points. '
  end

  scenario 'user enters non-integer' do
    visit new_assay_path
    fill_in('assay_remove_points', :with=>'1.2')
    click_button 'Create Assay'
    expect(page).to have_text 'Remove points - Please Enter a comma-separated list of points. '
  end

  scenario 'user enters ! seperated integers' do
    visit new_assay_path
    fill_in('assay_remove_points', :with=>'1!2!3!5!2')
    click_button 'Create Assay'
    expect(page).to have_text 'Remove points - Please Enter a comma-separated list of points. '
  end
  
  scenario 'array ends with comma' do
    visit new_assay_path
    fill_in('assay_remove_points', :with=>'1,2,')
    click_button 'Create Assay'
    expect(page).not_to have_text 'Remove points - Please Enter a comma-separated list of points. '
  end

end

