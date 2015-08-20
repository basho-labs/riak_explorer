import DS from 'ember-data';

var ObjectMetadata = DS.Model.extend({
    headers: DS.attr(null, {
        defaultValue: {
            custom: [],     // x-riak-meta-*
            indexes: [],    // x-riak-index-*
            other: {}       // everything else
        }
    }),

    causalContext: function() {
        return this.get('headers').other['x-riak-vclock'];
    }.property('headers'),

    contentType: function() {
        return this.get('headers').other['content-type'];
    }.property('headers'),

    dateLastModified: function() {
        return this.get('headers').other['last-modified'];
    }.property('headers'),

    // When this object was loaded from Riak via an HTTP request
    dateLoaded: function() {
        return this.get('headers').other['date'];
    }.property('headers'),

    etag: function() {
        return this.get('headers').other['etag'];
    }.property('headers'),

    headersCustom: function() {
        return this.get('headers').custom;
    }.property('headers'),

    /**
    * Return the necessary headers when saving an object via HTTP PUT
    */
    headersForUpdate: function() {
        // Start with the causal context
        var headers = {
            'X-Riak-Vclock': this.get('causalContext')
        };
        var header;
        var i;
        // Add the 2i indexes, if applicable
        var indexes = this.get('headersIndexes');
        for (i = 0; i < indexes.length; i++) {
            header = indexes[i];
            headers[header.key] = header.value;
        }
        // Add the user-defined custom headers
        var customHeaders = this.get('headersCustom');
        for (i = 0; i < customHeaders.length; i++) {
            header = customHeaders[i];
            headers[header.key] = header.value;
        }
        return headers;
    }.property('headers'),

    headersIndexes: function() {
        return this.get('headers').indexes;
    }.property('headers'),

    isDeleted: function() {
        return this.get('headers').other['x-riak-deleted'];
    }.property('headers')
});

export default ObjectMetadata;
