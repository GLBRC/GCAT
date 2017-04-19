require 'rails_helper'

feature 'User fills out growth threshold' do
  scenario 'default is 0.05' do
    visit new_assay_path
    expect(page).to have_field('assay_growth_threshold', :with=>'0.05')
  end

  scenario 'only accepts positive value' do
    visit new_assay_path
    expect(find('#assay_growth_threshold')[:min]).to eq '0'
  end
end

