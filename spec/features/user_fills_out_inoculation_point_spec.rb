require 'rails_helper'

feature 'User fills out inoculation point' do
  scenario 'default is 1' do
    visit new_assay_path
    expect(page).to have_field('assay_start_index', :with=>'1')
  end


   scenario 'only accepts positive value' do
     visit new_assay_path
     expect(find('#assay_start_index')[:min]).to eq '1'
   end
end

