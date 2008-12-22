class MessagesController < BaseController

  before_filter :enable_chat
  skip_before_filter :verify_authenticity_token

  # GET /messages
  # GET /messages.xml
  def index
    @messages = ChatMessage.to_user(current_user).last_week.all
    
    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @messages }
    end
  end

  # GET /messages/1
  # GET /messages/1.xml
  def show
    @message = ChatMessage.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @message }
    end
  end

  # POST /messages
  # POST /messages.xml
  def create
    @message = ChatMessage.new(:receiver_id => params[:receiver_id],
                               :message => params[:messageText],
                               :sender => current_user)

    if @message.save
      send_message
    end

    render :nothing => true

  end

  protected

  def send_message
    formatted_message = render_to_string(:partial => "message_for_chat", :object => @message)
    shooter_action_for_receiver = render_to_string :update do |page|
      page.call "showMessage", @message.sender.to_param, formatted_message
    end
    shooter_action_for_sender = render_to_string :update do |page|
      page.call "showMessage", @message.receiver.to_param, formatted_message
      page.call "messageTextBox.reset"
    end
    Meteor.shoot 'futura-chat', shooter_action_for_sender, [@message.sender.login]
    Meteor.shoot 'futura-chat', shooter_action_for_receiver, [@message.receiver.login]
  end
end
