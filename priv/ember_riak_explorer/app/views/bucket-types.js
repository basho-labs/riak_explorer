import Ember from 'ember';


export default Ember.View.extend({
   didInsertElement: function(){
    this.$("tr:even").css("background-color", "#efefef");
    this.$("tr:odd").css("background-color", "#ffffff");
  }
});