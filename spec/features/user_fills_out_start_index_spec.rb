require 'rails_helper'

feature 'User fills out start index', js: true do
  scenario 'it changes to 2 if it is 1 and user choose to use the first value as OD blank' do
    visit new_assay_path
    fill_in 'assay_start_index', with: '1'
    choose('assay_blank_value_default')
    expect(find_field('assay_start_index').value).to eq '2'
  end
  
  scenario 'it should not change start index if it is not 1 when user choose to use the first value as OD blank' do
    visit new_assay_path
    fill_in 'assay_start_index', with: '3'
    choose('assay_blank_value_default')
    expect(find_field('assay_start_index').value).to eq '3'
  end

end

