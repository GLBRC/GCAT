require 'rails_helper'

feature 'User fills out auc' do #, js: true do
  scenario 'with start and end equal to each other' do
    visit new_assay_path
    fill_in 'assay_area_start_hour', with: '1'
    fill_in 'assay_area_end_hour', with: '1'
    click_button 'Create Assay'
    expect(page).to have_content 'Area end hour must be greater than start hour.'
    expect(page).to have_content 'Area start hour must be less than end hour.'
  end

  scenario 'with start greater than end' do
    visit new_assay_path
    fill_in 'assay_area_start_hour', with: '100'
    fill_in 'assay_area_end_hour', with: '0'
    click_button 'Create Assay'
    expect(page).to have_content 'Area end hour must be greater than start hour.'
    expect(page).to have_content 'Area start hour must be less than end hour.'
  end

  scenario 'with start and/or end less than zero' do
    visit new_assay_path
    fill_in 'assay_area_start_hour', with: '-1'
    fill_in 'assay_area_end_hour', with: '-1'
    click_button 'Create Assay'
    expect(page).to have_content 'Area start hour - Please Enter a positive real number.'
    expect(page).to have_content 'Area end hour - Please Enter a positive real number.'
  end

=begin
#time consuming
  
  scenario 'with valid start and no end' do
    visit new_assay_path
    fill_in_valid_defaults
    fill_in 'assay_area_start_hour', with: '1'
    click_button 'Create Assay'
    expect(page).to have_selector '.alert', text: 'Your assay processed!'
  end

  scenario 'with valid start and end' do
    visit new_assay_path
    fill_in_valid_defaults
    fill_in 'assay_area_start_hour', with: '1'
    fill_in 'assay_area_start_hour', with: '2'
    click_button 'Create Assay'
    expect(page).to have_selector '.alert', text: 'Your assay processed!'
  end
=end

  # This will fill out the assay form with minimal needed to succeed. Should
  # always be filling out the area under curve in tests above, not here.
  def fill_in_valid_defaults
    attach_file('assay_input_file', Rails.root + 'test/fixtures/single - YPDAFEXglucoseTests_2-25-10.csv')
    choose('assay_blank_value_zero')
  end

end

