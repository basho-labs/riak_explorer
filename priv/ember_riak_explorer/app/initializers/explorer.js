export function initialize(container, app) {
    app.inject('route', 'explorer', 'service:explorer');
}

export default {
    name: 'explorer',
    initialize: initialize
};
