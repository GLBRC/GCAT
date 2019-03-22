source 'http://rubygems.org'

gem 'rails', '3.2.15'
gem 'jquery-rails'
gem 'rinruby'
gem 'rubyzip'
gem 'pry', :group=>:development
gem 'rake', :group=>:test

# Gems used only for assets and not required in production environments by default
group :assets do
  gem 'sass-rails',   '~> 3.2.3'
  gem 'uglifier'
  # See https://github.com/sstephenson/execjs#readme for more supported runtimes
  gem 'therubyracer', :platform => :ruby
end

group :test do
  # Pretty printed test output
  gem 'turn', :require => false
end

group :development do
  gem 'thin', '1.6.4'
  gem 'quiet_assets', '1.1.0'
end

group :production, :ci do
  gem 'passenger'
  gem 'mysql2', '0.4.10'
end

group :development, :test do
  gem 'sqlite3'
  gem 'rspec-rails', '~> 3.0'
  gem 'capybara', '2.4.0'
  gem 'poltergeist', '1.6.0'
  gem 'spring'
  gem 'spring-commands-rspec'
end
